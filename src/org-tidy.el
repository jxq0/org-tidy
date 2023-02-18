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

(defvar-local org-tidy-overlays-properties nil
  "Variable to store the regions we put an overlay on.")

(define-fringe-bitmap
  'org-tidy-fringe-bitmap-sharp
  [#b00010010
   #b00010010
   #b01111111
   #b00100100
   #b00100100
   #b11111110
   #b01001000
   #b01001000])

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

(defun org-tidy-overlay-properties (beg end)
  "Hides a region by making an invisible overlay over it."
  (interactive)
  (let* ((ovly-beg (1- beg))
         (ovly-end (1- end))
         (read-only-begin (max 1 ovly-beg))
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
          org-tidy-overlays)))

(defun org-tidy-properties-single (element)
  (-let* (((type props content) element)
          ((&plist :begin begin :end end) props))
    (org-tidy-overlay-properties begin end)))

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
          (ovly-beg-src (make-overlay ovly-beg-src-beg ovly-beg-src-end nil t nil))

          (ovly-end-src-beg (progn
                              (goto-char beg)
                              (goto-char (line-end-position))
                              (forward-char)
                              (+ (length value) (point))))
          (ovly-end-src-end (progn
                              (goto-char ovly-end-src-beg)
                              (goto-char (line-end-position))
                              (point)))
          (ovly-end-src (make-overlay ovly-end-src-beg ovly-end-src-end nil t nil))
          )
    (overlay-put ovly-beg-src 'display org-tidy-begin-src-symbol)
    (overlay-put ovly-end-src 'display org-tidy-end-src-symbol)

    (push (list :type 'beg-src :ov ovly-beg-src)
          org-tidy-overlays)
    (push (list :type 'end-src :ov ovly-end-src)
          org-tidy-overlays)))

(defun org-tidy-src ()
  "Tidy source blocks."
  (interactive)
  (save-excursion
    (org-element-map (org-element-parse-buffer)
        'src-block #'org-tidy-src-single)))

(defun org-untidy ()
  "Untidy."
  (interactive)
  (while org-tidy-overlays
    (-let* ((item (pop org-tidy-overlays))
            ((&plist :ov ov
                     :type type
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
      (remove-text-properties del-beg del-end '(local-map nil)))))

(defun org-tidy ()
  "Tidy."
  (interactive)
  ;; (save-excursion
  ;;   (goto-char (point-min))
  ;;   (while (re-search-forward org-property-drawer-re nil t)
  ;;       (let* ((beg (match-beginning 0))
  ;;              (end (1+ (match-end 0))))
  ;;         (org-tidy-hide beg end))))
  )

(provide 'org-tidy)

;;; org-tidy.el ends here
