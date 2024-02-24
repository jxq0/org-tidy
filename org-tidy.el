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
  "Non-nil means should tidy property drawers."
  :group 'org-tidy
  :type '(choice
          (const :tag "Tidy property drawers" t)
          (const :tag "Keep property drawers" nil)))

(defcustom org-tidy-property-drawer-property-whitelist ()
  "Whitelist of properties.
If set, only property drawers which contain property in this list
 will be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-property-drawer-property-blacklist ()
  "Blacklist of properties.
If set, property drawers which contain property in this list
will not be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-general-drawer-flag t
  "Non-nil means should tidy general drawers."
  :group 'org-tidy
  :type '(choice
          (const :tag "Tidy general drawers" t)
          (const :tag "Keep general drawers" nil)))

(defcustom org-tidy-general-drawer-name-whitelist ()
  "Whitelist of drawer names.
If set, only general drawers whose name is in this list
 will be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-general-drawer-name-blacklist ()
  "Blacklist of drawer names.
If set, general drawers whose name is in this list
will not be tidied."
  :group 'org-tidy
  :type '(repeat string))

(defcustom org-tidy-protect-overlay t
  "If non-nil, org-tidy will protect the overlay by changing local-map."
  :group 'org-tidy
  :type 'boolean)

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
  "Return t if the property drawer ELEMENT contain a key in CHECK-LIST.
Otherwise return nil."
  (-let* ((l (cddr element)))
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
  "Return t if the general drawer ELEMENT contain a key in CHECK-LIST.
Otherwise return nil."
  (-let* ((content (cadr element))
          (drawer-name (plist-get content :drawer-name)))
    (if (member drawer-name check-list)
        t)))

(defun org-tidy-should-tidy (element)
  "Return whether ELEMENT should be tidied."
  (-let* ((type (car element)))
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

(defun org-tidy--element-to-ov (element)
  "Turn a single property ELEMENT into a plist for merge."
  (let* ((should-tidy (org-tidy-should-tidy element))
         (beg (org-element-property :begin element))
         (end (org-element-property :end element))
         (is-top-property (= 1 beg))
         (push-ovly nil)
         (display nil))

    (pcase (list is-top-property org-tidy-top-property-style
                 org-tidy-properties-style)
      (`(t invisible ,_)
       (setq display 'empty push-ovly t))

      (`(t keep ,_) )

      (`(nil ,_ invisible)
       (setq display 'empty push-ovly t))

      (`(nil ,_ inline)
       (setq display 'inline-symbol push-ovly t))

      (`(nil ,_ fringe)
       (setq display 'fringe push-ovly t)))

    (when (and should-tidy push-ovly)
      (list :beg beg
            :end end
            :is-top-property is-top-property
            :display display))))

(defun org-tidy--merge-raw-ovs (raw-ovs)
  "Merge adjacent RAW-OVS."
  (let* ((result nil))
    (while raw-ovs
      (let* ((curr (car raw-ovs))
             (curr-beg (plist-get curr :beg))
             (curr-end (plist-get curr :end))
             (last (car result))
             (last-end (plist-get last :end)))
        (if (and last (= curr-beg last-end))
            (setf (car result) (plist-put last :end curr-end))
          (push curr result)))
      (setq raw-ovs (cdr raw-ovs)))
    result))

(defun org-tidy--calc-ovly (merged-ovs)
  "Calculate overlay and protect regions for MERGED-OVS."
  (mapcar (lambda (l)
            (-let* (((&plist :beg
                             :end
                             :is-top-property) l)
                    (ovly-beg (if is-top-property 1 (1- beg)))
                    (ovly-end (if is-top-property end (1- end)))
                    (backspace-beg (1- end))
                    (backspace-end end)
                    (del-beg (max 1 (1- beg)))
                    (del-end (1+ del-beg)))
              (append l (list :ovly-beg ovly-beg
                              :ovly-end ovly-end
                              :backspace-beg backspace-beg
                              :backspace-end backspace-end
                              :del-beg del-beg
                              :del-end del-end))))
          merged-ovs))

(defun org-tidy--put-overlays (ovs)
  "Put overlays from OVS, ensuring newline after drawer is kept."
  (dolist (l ovs)
    (-when-let* (((&plist :ovly-beg :ovly-end :display
                          :backspace-beg :backspace-end
                          :del-beg :del-end) l)
                 (not-exists (not (org-tidy-overlay-exists ovly-beg ovly-end)))
                 ;; Adjust ovly-end to keep newline after drawer
                 (adjusted-ovly-end (if
		 ;; check i there is a newline after
(save-excursion
    (goto-char ovly-end)
    (looking-at-p "\n"))


                                        (1- ovly-end)
                                      ovly-end))
                 (ovly (make-overlay ovly-beg adjusted-ovly-end nil t nil)))
(pcase display
        ('empty (overlay-put ovly 'display ""))

        ('inline-symbol
         (overlay-put ovly 'display
                      (format " %s" org-tidy-properties-inline-symbol)))

        ('fringe
         (overlay-put ovly 'display
                      '(left-fringe org-tidy-fringe-bitmap-sharp org-drawer))))

      (push (list :type 'property :ov ovly) org-tidy-overlays)

      (org-tidy-make-protect-ov backspace-beg backspace-end
                                del-beg del-end)
    )))

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
    (let* ((raw-ovs (org-element-map (org-element-parse-buffer)
                    '(property-drawer drawer)
                    #'org-tidy--element-to-ov)))
      (org-tidy--put-overlays
       (org-tidy--calc-ovly
        (org-tidy--merge-raw-ovs raw-ovs))))))

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
