;;; org-tidy.el --- A minor mode to tidy org-mode buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Xuqing Jia

;; Author: Xuqing Jia <jxq@jxq.me>
;; URL: https://github.com/jxq0/org-tidy
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.19.1"))
;; Keywords: convenience, org

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A minor mode to tidy org-mode buffers.

(require 'org)
(require 'org-element)
(require 'dash)

;;; Code:

(defgroup org-tidy nil
  "A minor mode to tidy `org-mode' buffers."
  :prefix "org-tidy-"
  :group 'convenience)

(defcustom org-tidy-properties-style 'inline
  "How to tidy property drawers."
  :group 'org-tidy
  :type '(choice
          (const :tag "Show fringe bitmap" fringe)
          (const :tag "Show inline symbol" inline)
          (const :tag "Completely invisible" invisible)))

(defcustom org-tidy-top-property-style 'invisible
  "How to tidy the topmost property drawer."
  :group 'org-tidy
  :type '(choice
          (const :tag "Completely invisible" invisible)
          (const :tag "Keep" keep)))

(defcustom org-tidy-properties-inline-symbol "♯"
  "The inline symbol."
  :group 'org-tidy
  :type 'string)

(defcustom org-tidy-property-drawer-flag t
  "Whether to tidy property drawers or not."
  :group 'org-tidy
  :type '(choice
          (const :tag "Tidy property drawers" t)
          (const :tag "Keep property drawers" nil)))

(defcustom org-tidy-property-drawer-property-whitelist ()
  "If t, and `org-tidy-property-drawer-flag' is t, only property drawers which contains property in this list will be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-property-drawer-property-blacklist ()
  "If t, and `org-tidy-property-drawer-flag' is t, property drawers which contains property in this list will not be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-general-drawer-flag t
  "Whether to tidy general drawers or not."
  :group 'org-tidy
  :type '(choice
          (const :tag "Tidy general drawers" t)
          (const :tag "Keep general drawers" nil)))

(defcustom org-tidy-general-drawer-name-whitelist ()
  "If t, and `org-tidy-general-drawer-flag' is t, only general drawers whose name is in this list will be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-general-drawer-name-blacklist ()
  "If t, and `org-tidy-general-drawer-flag' is t, general drawers whose name is in this list will not be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defun org-tidy-protected-text-edit ()
  "Keymap to protect property drawers."
  (interactive)
  (user-error "Property drawer is protected in org-tidy mode"))

(defvar org-tidy-properties-backspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'org-tidy-protected-text-edit)
    map)
  "Keymap to protect property drawers.")

(defvar org-tidy-properties-delete-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") #'org-tidy-protected-text-edit)
    (define-key map (kbd "<deletechar>") #'org-tidy-protected-text-edit)
    map)
  "Keymap to protect property drawers.")

(defvar-local org-tidy-overlays nil
  "Variable to store the regions we put an overlay on.")

(defvar-local org-tidy-toggle-state t
  "Variable to control whether this buffer should be tidied.")

(define-fringe-bitmap
  'org-tidy-fringe-bitmap-sharp
  [#b00100100
   #b00100100
   #b11111111
   #b00100100
   #b00100100
   #b11111111
   #b00100100
   #b00100100])

(defun org-tidy-overlay-exists (ovly-beg ovly-end)
  "Check whether overlay from OVLY-BEG to OVLY-END exists."
  (-filter (lambda (item)
             (let* ((ov (plist-get item :ov))
                    (old-ovly-beg (overlay-start ov))
                    (old-ovly-end (overlay-end ov)))
               (and (= ovly-beg old-ovly-beg)
                    (>= ovly-end old-ovly-end))))
           org-tidy-overlays))

(defun org-tidy-make-protect-ov (backspace-beg backspace-end del-beg del-end)
  "Make two read-only overlay: (BACKSPACE-BEG, BACKSPACE-END) (DEL-BEG, DEL-END)."
  (let* ((backspace-ov (make-overlay backspace-beg backspace-end nil t t))
         (del-ov (make-overlay del-beg del-end nil t nil)))
    (overlay-put backspace-ov
                 'local-map org-tidy-properties-backspace-map)
    (overlay-put del-ov
                 'local-map org-tidy-properties-delete-map)

    (push (list :type 'protect :ov backspace-ov) org-tidy-overlays)
    (push (list :type 'protect :ov del-ov) org-tidy-overlays)))

(defun org-tidy-property-drawer-has-key-in-list (element check-list)
  "Return t if property drawer contains a key in CHECK-LIST, otherwise return nil."
  (-let* (((type content . l) element))
    (when-let* ((check-list)
                (not-hit t))
      (while (and l not-hit)
        (-let* ((element (car l))
                ((type content) element))
          (when (eq type 'node-property)
            (if (member (plist-get content :key) check-list)
                (setq not-hit nil)))
          (setq l (cdr l))))
      (not not-hit))))

(defun org-tidy-general-drawer-name-in-list (element check-list)
  "Return t if property drawer contains a key in CHECK-LIST, otherwise return nil."
  (-let* (((type content . l) element)
          (drawer-name (plist-get content :drawer-name)))
    (if (member drawer-name check-list)
        t)))

(defun org-tidy-should-tidy (element)
  (-let* (((type content . children) element))
    (pcase type
      ('drawer
       (and org-tidy-general-drawer-flag
            (if org-tidy-general-drawer-name-whitelist
                (org-tidy-general-drawer-name-in-list
                 element
                 org-tidy-general-drawer-name-whitelist)
              (not (org-tidy-general-drawer-name-in-list
                    element
                    org-tidy-general-drawer-name-blacklist)))))
      ('property-drawer
       (and org-tidy-property-drawer-flag
            (if org-tidy-property-drawer-property-whitelist
                (org-tidy-property-drawer-has-key-in-list
                 element org-tidy-property-drawer-property-whitelist)
              (not (org-tidy-property-drawer-has-key-in-list
                    element
                    org-tidy-property-drawer-property-blacklist))))))))

(defun org-tidy-properties-single (element)
  "Tidy a single property ELEMENT."
  (-let* (((type content . children) element)
          (should-tidy (org-tidy-should-tidy element))
          ((&plist :begin beg :end end) content)
          (is-top-property (= 1 beg))
          (ovly-beg (if is-top-property 1 (1- beg)))
          (ovly-end (if is-top-property end (1- end))))
    (when (and should-tidy
               (not (org-tidy-overlay-exists ovly-beg ovly-end)))
      (let* ((backspace-beg (1- end))
             (backspace-end end)
             (del-beg (max 1 (1- beg)))
             (del-end (1+ del-beg))
             (ovly (make-overlay ovly-beg ovly-end nil t nil))
             (push-ovly nil))
        (pcase (list is-top-property
                     org-tidy-top-property-style
                     org-tidy-properties-style)
          (`(t invisible ,_)
           (overlay-put ovly 'display "")
           (setf push-ovly t))
          (`(t keep ,_) (delete-overlay ovly))
          (`(nil ,_ inline)
           (overlay-put ovly 'display
                        (format " %s" org-tidy-properties-inline-symbol))
           (setf push-ovly t))
          (`(nil ,_ fringe)
           (overlay-put ovly 'display
                        '(left-fringe org-tidy-fringe-bitmap-sharp org-drawer))
           (setf push-ovly t)))

        (when push-ovly
          (push (list :type 'property
                      :ov ovly)
                org-tidy-overlays)

          (org-tidy-make-protect-ov backspace-beg backspace-end
                                    del-beg del-end))))))

(defun org-tidy-untidy-buffer ()
  "Untidy."
  (interactive)
  (while org-tidy-overlays
    (-let* ((item (pop org-tidy-overlays))
            ((&plist :type type) item))
      (pcase type
        ('property (delete-overlay (plist-get item :ov)))
        ('protect (delete-overlay (plist-get item :ov)))
        (_ nil)))))

(defun org-tidy-buffer ()
  "Tidy."
  (interactive)
  (save-excursion
    (org-element-map (org-element-parse-buffer)
        '(property-drawer drawer) #'org-tidy-properties-single)))

(defun org-tidy-toggle ()
  "Toggle between tidy and untidy."
  (interactive)
  (if org-tidy-toggle-state
      (progn (setq org-tidy-toggle-state nil)
             (org-tidy-untidy-buffer))
    (progn (setq org-tidy-toggle-state t)
           (org-tidy-buffer))))

(defun org-tidy-on-save ()
  "Tidy buffer on save if `org-tidy-toggle-state' is t."
  (interactive)
  (if org-tidy-toggle-state (org-tidy-buffer)))

;;;###autoload
(define-minor-mode org-tidy-mode
  "Automatically tidy org mode buffers."
  :global nil
  :group 'org-tidy
  (if org-tidy-mode
      (progn
        (if (eq org-tidy-properties-style 'fringe)
            (let* ((width 10))
              (setq left-fringe-width width)
              (set-window-fringes nil width)))
        (org-tidy-buffer)
        (add-hook 'before-save-hook #'org-tidy-on-save nil t))
    (progn
      (if (eq org-tidy-properties-style 'fringe)
          (progn (setq left-fringe-width nil)
                 (set-window-fringes nil nil)))
      (org-tidy-untidy-buffer)
      (remove-hook 'before-save-hook #'org-tidy-on-save t))))

(provide 'org-tidy)

;;; org-tidy.el ends here
