#!/usr/bin/env emacs --script

;; Test script for citation folding functionality

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

;; Load the grammar and mode
(load-file (expand-file-name "grgfoo.el"))

;; Test if the mode can be activated
(with-temp-buffer
  (insert-file-contents (expand-file-name "test-folding.greger"))
  (grgfoo-mode)
  (message "Mode activated successfully")

  ;; Check if font-lock is working
  (font-lock-ensure)
  (message "Font-lock enabled")

  ;; Try to find a citation
  (goto-char (point-min))
  (when (search-forward "Einstein developed" nil t)
    (message "Found citation text at position %d" (point)))

  ;; Try to find citations section
  (goto-char (point-min))
  (when (search-forward "## CITATIONS:" nil t)
    (message "Found citations section at position %d" (point)))

  (message "Test completed successfully"))
