;;; org-tidy.el --- A minor mode to tidy org-mode buffers.

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

(defcustom org-tidy-top-property-style 'hide
  "If non-nil, add text properties to the region markers."
  :group 'org-tidy
  :type '(choice
          (const :tag "Hide completely" hide)
          (const :tag "Keep" keep)))

(defcustom org-tidy-properties-inline-symbol "♯"
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
          (is-top-property (= 1 beg))
          (ovly-beg (if is-top-property 1 (1- beg)))
          (ovly-end (if is-top-property end (1- end))))
    (unless (org-tidy-overlay-exists ovly-beg ovly-end)
      (let* ((read-only-begin ovly-beg)
             (read-only-end end)
             (backspace-beg (1- end))
             (backspace-end end)
             (del-beg (max 1 (1- beg)))
             (del-end (1+ del-beg))
             (ovly (make-overlay ovly-beg ovly-end nil t nil))
             (push-ovly nil))
        (pcase (list is-top-property
                     org-tidy-top-property-style
                     org-tidy-properties-style)
          (`(t hide ,_)
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
                org-tidy-overlays))))))

(defun org-tidy-properties ()
  "Tidy drawers."
  (save-excursion
    (org-element-map (org-element-parse-buffer)
        'property-drawer #'org-tidy-properties-single)))

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

(defun org-untidy-buffer ()
  "Untidy."
  (interactive)
  (while org-tidy-overlays
    (-let* ((item (pop org-tidy-overlays))
            ((&plist :type type) item))
      (pcase type
        ('property (org-untidy-property item))
        (_ nil)))))

(defun org-tidy-buffer ()
  "Tidy."
  (interactive)
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
      (org-untidy-buffer)
      (remove-hook 'before-save-hook 'org-tidy-buffer t))))

(provide 'org-tidy)

;;; org-tidy.el ends here
