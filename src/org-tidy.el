;;; org-stealth.el --- A minor mode to clean org-mode.

;;; Commentary:
;;

(require 'org)

;;; Code:

(defgroup hide-region nil
  "Functions to hide region using an overlay with the invisible property.  The text is not affected."
  :prefix "hide-region-"
  :group 'convenience)

(defcustom hide-region-before-string "@["
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay"
  :type 'string
  :group 'hide-region)

(defcustom hide-region-after-string "]@"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay"
  :type 'string
  :group 'hide-region)

(defcustom hide-region-propertize-markers t
  "If non-nil, add text properties to the region markers."
  :type 'boolean
  :group 'hide-region)

(defface hide-region-before-string-face
  '((t (:inherit region)))
  "Face for the before string.")

(defface hide-region-after-string-face
  '((t (:inherit region)))
  "Face for the after string.")

(defvar hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

;;;###autoload
(defun hide-region-hide (beg end)
  "Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\""
  (interactive)
  (let ((new-overlay (make-overlay beg end)))
    (push new-overlay hide-region-overlays)
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    ;; (overlay-put new-overlay 'display '((margin left-margin) "x"))
    (overlay-put new-overlay 'display '(left-fringe flycheck-fringe-bitmap-double-arrow))
    ;; (overlay-put new-overlay 'before-string
    ;;              (if hide-region-propertize-markers
    ;;                  (propertize hide-region-before-string
    ;;                              'font-lock-face 'hide-region-before-string-face
    ;;                              'display '((margin left-margin) "◎"))
    ;;                hide-region-before-string))
    ;; (overlay-put new-overlay 'after-string
    ;;              (if hide-region-propertize-markers
    ;;                  (propertize hide-region-after-string
    ;;                              'font-lock-face 'hide-region-after-string-face)
    ;;                hide-region-after-string))
    ))

;;;###autoload
(defun hide-region-unhide ()
  "Unhide a region at a time, starting with the last one hidden and
deleting the overlay from the hide-region-overlays \"ring\"."
  (interactive)
  (when (car hide-region-overlays)
    (delete-overlay (car hide-region-overlays))
    (setq hide-region-overlays (cdr hide-region-overlays))))

(defun org-tidy ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((regions '()))
      (while (re-search-forward org-property-drawer-re nil t)
        (let* ((beg (match-beginning 0))
               (end (1+ (match-end 0))))
          (hide-region-hide beg end)
          (push (list beg end) regions)))
      regions)))

(provide 'org-tidy)

;;; org-stealth.el ends here
