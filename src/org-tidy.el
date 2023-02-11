;;; org-stealth.el --- A minor mode to clean org-mode.

;;; Commentary:
;;

(require 'org)

;;; Code:

(defgroup org-tidy nil
  "Give you a clean org-mode buffer."
  :prefix "org-tidy-"
  :group 'convenience)

(defcustom org-tidy-properties 'name
  "If non-nil, add text properties to the region markers."
  :group 'org-tidy
  :type '(choice
          (const :tag "Only name" name)
          (const :tag "Abbreviated path" abbreviate)
          (const :tag "Full path" full)
          ))

(defcustom org-tidy-src-block t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'org-tidy)

(defvar-local org-tidy-overlays nil
  "Variable to store the regions we put an overlay on.")

(defun org-tidy-hide (beg end)
  "Hides a region by making an invisible overlay over it."
  (interactive)
  (unless (assoc (list beg end) org-tidy-overlays)
    (let ((new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'intangible t)
      (overlay-put new-overlay 'display
                   '(left-fringe flycheck-fringe-bitmap-double-arrow))
      (push (cons (list beg end) new-overlay) org-tidy-overlays))))

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
      ;; res
      )))

(defun org-untidy ()
  "Untidy."
  (interactive)
  (while org-tidy-overlays
    (let* ((ov (cdar org-tidy-overlays)))
      (message "ov:%s" ov)
      (delete-overlay ov)
      (setf org-tidy-overlays (cdr org-tidy-overlays)))))

(defun org-tidy ()
  "Tidy."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-property-drawer-re nil t)
        (let* ((beg (match-beginning 0))
               (end (1+ (match-end 0))))
          (org-tidy-hide beg end)))))

(provide 'org-tidy)

;;; org-tidy.el ends here
