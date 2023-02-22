;;; org-stealth.el --- A minor mode to clean org-mode.

;;; Commentary:
;;

(require 'org)
(require 'org-element)
(require 'dash)

;;; Code:

(defgroup org-tidy nil
  "Give you a clean org-mode buffer."
  :prefix "org-tidy-"
  :group 'convenience)

(defcustom org-tidy-properties t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'org-tidy)

(defcustom org-tidy-properties-style 'inline
  "If non-nil, add text properties to the region markers."
  :group 'org-tidy
  :type '(choice
          (const :tag "Only show fringe bitmap" fringe)
          (const :tag "Only show inline symbol" inline)
          (const :tag "Show nothing" nothing)))

(defcustom org-tidy-properties-inline-symbol "♯"
  "docstring"
  :type 'string)

(defcustom org-tidy-src-block t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'org-tidy)

(defcustom org-tidy-begin-src-symbol "☰"
  "docstring"
  :type 'string)

(defcustom org-tidy-end-src-symbol "☰"
  "docstring"
  :type 'string)

(defun org-tidy-protected-text-edit ()
  (interactive)
  (user-error "Text is protected."))

(defvar org-tidy-properties-backspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") #'org-tidy-protected-text-edit)
    map)
  "keymap for property drawers")

(defvar org-tidy-properties-delete-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") #'org-tidy-protected-text-edit)
    (define-key map (kbd "<deletechar>") #'org-tidy-protected-text-edit)
    map)
  "keymap for property drawers")

(defvar-local org-tidy-overlays nil
  "Variable to store the regions we put an overlay on.
:property-beg-offset is begin of property minus begin of overlay.
:property-end-offset is end of property minus end of overlay.")

;; (define-fringe-bitmap
;;   'org-tidy-fringe-bitmap-sharp
;;   [#b00010010
;;    #b00010010
;;    #b01111111
;;    #b00100100
;;    #b00100100
;;    #b11111110
;;    #b01001000
;;    #b01001000])

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
  "docstring"
  (-filter (lambda (item)
             (let* ((ov (plist-get item :ov))
                    (old-ovly-beg (overlay-start ov))
                    (old-ovly-end (overlay-end ov)))
               (and (= ovly-beg old-ovly-beg)
                    (= ovly-end old-ovly-end))))
           org-tidy-overlays))

(defun org-tidy-overlay-properties-test (beg end)
  "Hides a region by making an invisible overlay over it."
  (interactive)
  (let* ((pl (list :ovly-beg (1- beg)
                   :ovly-end (1- end)
                   :backspace-beg (1- end)
                   :backspace-end end
                   :del-beg (1- beg)
                   :del-end beg)))
    (message "%s" pl)))

(defun org-tidy-properties-single (element)
  (-let* (((type props content) element)
          ((&plist :begin beg :end end) props)
          (ovly-beg (max 1 (1- beg)))
          (ovly-end (1- end)))
    (unless (org-tidy-overlay-exists ovly-beg ovly-end)
      (let* ((read-only-begin ovly-beg)
             (read-only-end end)
             (backspace-beg (1- end))
             (backspace-end end)
             (del-beg (max 1 (1- beg)))
             (del-end (1+ del-beg))
             (ovly (make-overlay ovly-beg ovly-end nil t nil)))
        (pcase org-tidy-properties-style
          ('inline
            (overlay-put ovly 'display
                         (format " %s" org-tidy-properties-inline-symbol)))
          ('fringe
           (overlay-put ovly 'display
                        '(left-fringe org-tidy-fringe-bitmap-sharp org-drawer))))

        (put-text-property backspace-beg backspace-end
                           'local-map org-tidy-properties-backspace-map)
        (put-text-property del-beg del-end
                           'local-map org-tidy-properties-delete-map)

        (push (list :type 'property
                    :ov ovly
                    :backspace-beg-offset (- ovly-end backspace-beg)
                    :backspace-end-offset (- ovly-end backspace-end)
                    :del-beg-offset (- ovly-beg del-beg)
                    :del-end-offset (- ovly-beg del-end))
              org-tidy-overlays)))))

(defun org-tidy-properties ()
  "Tidy drawers."
  (interactive)
  (save-excursion
    (org-element-map (org-element-parse-buffer)
        'property-drawer #'org-tidy-properties-single)))

(defun org-tidy-src-single (src-block)
  (-let* (((type props content) src-block)
          ((&plist :begin beg :end end :value value) props)
          (ovly-beg-src-beg beg)
          (ovly-beg-src-end (+ 11 beg))
          (ovly-end-src-beg (progn
                              (goto-char beg)
                              (goto-char (line-end-position))
                              (forward-char)
                              (+ (length value) (point))))
          (ovly-end-src-end (progn
                              (goto-char ovly-end-src-beg)
                              (goto-char (line-end-position))
                              (point))))
    (unless (or (org-tidy-overlay-exists ovly-beg-src-beg ovly-beg-src-end)
                (org-tidy-overlay-exists ovly-end-src-beg ovly-end-src-end))
      (let* ((ovly-beg-src (make-overlay ovly-beg-src-beg
                                         ovly-beg-src-end nil t nil))
             (ovly-end-src (make-overlay ovly-end-src-beg
                                         ovly-end-src-end nil t nil)))
        (overlay-put ovly-beg-src 'display org-tidy-begin-src-symbol)
        (overlay-put ovly-end-src 'display org-tidy-end-src-symbol)

        (push (list :type 'beg-src :ov ovly-beg-src)
              org-tidy-overlays)
        (push (list :type 'end-src :ov ovly-end-src)
              org-tidy-overlays)))))

(defun org-tidy-src ()
  "Tidy source blocks."
  (interactive)
  (save-excursion
    (org-element-map (org-element-parse-buffer)
        'src-block #'org-tidy-src-single)))

(defun org-untidy-property (item)
  (-let* (((&plist :ov ov
                   :backspace-beg-offset backspace-beg-offset
                   :backspace-end-offset backspace-end-offset
                   :del-beg-offset del-beg-offset
                   :del-end-offset del-end-offset)
           item)
          (backspace-beg (- (overlay-end ov) backspace-beg-offset))
          (backspace-end (- (overlay-end ov) backspace-end-offset))
          (del-beg (- (overlay-start ov) del-beg-offset))
          (del-end (- (overlay-start ov) del-end-offset)))
    (delete-overlay ov)
    (remove-text-properties backspace-beg backspace-end '(local-map nil))
    (remove-text-properties del-beg del-end '(local-map nil))))

(defun org-untidy-src (item)
  (-let* (((&plist :ov ov) item))
    (delete-overlay ov)))

(defun org-untidy-buffer ()
  "Untidy."
  (interactive)
  (while org-tidy-overlays
    (-let* ((item (pop org-tidy-overlays))
            ((&plist :type type) item))
      (pcase type
        ('property (org-untidy-property item))
        (_ (org-untidy-src item))))))

(defun org-tidy-buffer ()
  "Tidy."
  (interactive)
  (if org-tidy-src-block (org-tidy-src))
  (if org-tidy-properties (org-tidy-properties)))

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
        (add-hook 'before-save-hook 'org-tidy-buffer nil t))
    (progn
      (if (eq org-tidy-properties-style 'fringe)
          (progn (setq left-fringe-width nil)
                 (set-window-fringes nil nil)))
      (org-untidy)
      (remove-hook 'before-save-hook 'org-tidy-buffer t))))

(provide 'org-tidy)

;;; org-tidy.el ends here
