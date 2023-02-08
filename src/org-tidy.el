;;; org-stealth.el --- A minor mode to clean org-mode.

;;; Commentary:
;;

(require 'org)

;;; Code:

(defgroup hide-region nil
  "Functions to hide region using an overlay with the invisible property.  The text is not affected."
  :prefix "hide-region-"
  :group 'convenience)

(defcustom hide-region-propertize-markers t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'hide-region)

(defvar org-tidy-overlays nil
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
