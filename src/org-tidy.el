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

(defvar-local org-tidy-properties-symbol "♯"
  "Variable to store the regions we put an overlay on.")

(defvar org-tidy-properties-backspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>")
      (lambda ()
        (interactive)
        (message "backspace")))
    map)
  "keymap for property drawers")

(defcustom org-tidy-src-block t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'org-tidy)

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
                   :backspace-end end)))
    (message "%s" pl)))

(defun org-tidy-overlay-properties (beg end)
  "Hides a region by making an invisible overlay over it."
  (interactive)
  (let* ((ov nil)
         (ovly-beg (1- beg))
         (ovly-end (1- end))
         (read-only-begin (max 1 ovly-beg))
         (read-only-end end)
         (backspace-beg (1- end))
         (backspace-end end)
         (ovly (make-overlay ovly-beg ovly-end nil t nil)))
    (pcase org-tidy-properties-style
      ('inline
        (overlay-put ovly 'display " ♯")
        (overlay-put ovly 'invisible t))

      ('fringe
       (overlay-put ovly 'display
                    '(left-fringe org-tidy-fringe-bitmap-sharp org-drawer))

       (put-text-property backspace-beg backspace-end
                          'local-map org-tidy-properties-backspace-map)
       ))
    (push (list :type 'property
                :ov ovly
                :backspace-beg-offset (- ovly-end backspace-beg)
                :backspace-end-offset (- ovly-end backspace-end))
          org-tidy-overlays)))

(defun org-tidy-properties-single (element)
  (-let* (((type props content) element)
          ((&plist :begin begin :end end) props))
    (org-tidy-overlay-properties begin end)))

(defun org-tidy-properties ()
  "Tidy drawers."
  (interactive)
  (save-excursion
    (let* ((res (org-element-map (org-element-parse-buffer)
                    'property-drawer #'org-tidy-properties-single)))
      res
      )))

(defun org-tidy-overlay-end-src (beg end)
  "Hides a region by making an invisible overlay over it."
  (interactive)
  (unless (assoc (list beg end) org-tidy-overlays)
    (let ((new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'display "☰")
      (push (cons (list beg end) new-overlay) org-tidy-overlays))))

(defun org-tidy-overlay-begin-src (beg end)
  "Hides a region by making an invisible overlay over it."
  (interactive)
  (unless (assoc (list beg end) org-tidy-overlays)
    (let ((new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'display "☰")
      (push (cons (list beg end) new-overlay) org-tidy-overlays))))

(defun org-tidy-src-single (src)
  (let* ((pl (cadr src))
         (begin (plist-get pl :begin))
         (end-src-beg (progn
                        (goto-char begin)
                        (goto-char (line-end-position))
                        (forward-char)
                        (+ (length (plist-get pl :value)) (point))))
         (end (progn (goto-char end-src-beg)
                     (goto-char (line-end-position))
                     (point)))
         )
    (list :begin begin :end-src-beg end-src-beg :end end)))



(defun org-tidy-src ()
  "Tidy source blocks."
  (interactive)
  (save-excursion
    (let* ((res (org-element-map (org-element-parse-buffer)
                    'src-block #'org-tidy-src-single)))
      (mapcar (lambda (item)
                (let* ((end-src-beg (plist-get item :end-src-beg))
                       (end (plist-get item :end))
                       (begin (plist-get item :begin)))
                  (org-tidy-overlay-end-src end-src-beg end)
                  (org-tidy-overlay-begin-src begin (+ 11 begin))))
              res)
      )))

(defun org-untidy ()
  "Untidy."
  (interactive)
  (while org-tidy-overlays
    (-let* ((item (pop org-tidy-overlays))
            ((&plist :ov ov
                     :type type
                     :backspace-beg-offset backspace-beg-offset
                     :backspace-end-offset backspace-end-offset)
             item)
            (backspace-beg (- (overlay-end ov) backspace-beg-offset))
            (backspace-end (- (overlay-end ov) backspace-end-offset)))
      (delete-overlay ov)
      (remove-text-properties backspace-beg backspace-end '(local-map nil))
      )))

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
