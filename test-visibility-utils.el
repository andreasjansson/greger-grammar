#!/usr/bin/env emacs --batch

;; Utility functions for testing text visibility

(defun buffer-visible-text (&optional buffer)
  "Return the visible text from BUFFER (or current buffer).
This includes text made visible by overlays and excludes invisible text."
  (with-current-buffer (or buffer (current-buffer))
    (let ((result "")
          (pos (point-min)))
      (while (< pos (point-max))
        (let* ((next-invisible (next-single-property-change pos 'invisible nil (point-max)))
               (next-after-string (next-single-property-change pos 'after-string nil (point-max)))
               (next-change (min next-invisible next-after-string))
               (invisible (get-text-property pos 'invisible))
               (visible (not (and invisible (invisible-p invisible)))))
          ;; Add visible text
          (when visible
            (setq result (concat result (buffer-substring pos next-change))))

          ;; Check for after-string property that adds visible text
          (let ((after-string (get-text-property pos 'after-string)))
            (when after-string
              (setq result (concat result after-string))))

          (setq pos next-change)))
      result)))

(defun debug-text-properties (start end)
  "Debug text properties in region from START to END."
  (let ((pos start))
    (while (< pos end)
      (let* ((next-change (next-single-property-change pos nil nil end))
             (props (text-properties-at pos)))
        (when props
          (message "Position %d-%d: %s" pos next-change props))
        (setq pos next-change)))))

;; Test the visibility function
(add-to-list 'treesit-extra-load-path default-directory)
(load-file "grgfoo.el")

(with-temp-buffer
  (insert "## ASSISTANT:\n\nTest text\n\n### https://example.com\n\nTitle: Test\nCited text: Example citation\n")
  (grgfoo-mode)
  (font-lock-ensure)

  (message "Original buffer content:")
  (message "%s" (buffer-string))

  (message "\nVisible buffer content:")
  (message "%s" (buffer-visible-text))

  (message "\nText properties debug:")
  (debug-text-properties (point-min) (point-max)))
