#!/usr/bin/env emacs --script
;;; test-segfault-batch.el --- Batch script to reproduce segfault safely

;; Load debugging aids
(setq debug-on-error t)
(setq debug-on-quit t)

;; Load the grgfoo mode
(message "Loading grgfoo.el...")
(condition-case err
    (load-file "grgfoo.el")
  (error (message "Error loading grgfoo.el: %s" err)))

;; Test 1: Check if treesit is available and greger grammar can be loaded
(message "Testing tree-sitter availability...")
(message "treesit-available-p: %s" (if (fboundp 'treesit-available-p) (treesit-available-p) "function not available"))

(when (fboundp 'treesit-ready-p)
  (message "treesit-ready-p for 'greger: %s" (treesit-ready-p 'greger)))

;; Test 2: Create a minimal test file and activate the mode
(message "Creating test file...")
(let ((test-file "test.grgfoo"))
  (condition-case err
      (progn
        (find-file test-file)
        (message "File created successfully")
        (insert "## USER:")
        (message "Text inserted")
        (condition-case mode-err
            (progn
              (grgfoo-mode)
              (message "Mode activated: %s" major-mode))
          (error (message "Error activating mode: %s" mode-err)))

        ;; Test the problematic action: hitting return
        (message "Testing newline insertion...")
        (condition-case newline-err
            (progn
              (goto-char (point-max))
              (newline)
              (message "Newline inserted successfully"))
          (error (message "Error inserting newline: %s" newline-err)))

        (message "Buffer content: %s" (buffer-string))
        (kill-buffer))
    (error (message "Error in test: %s" err))))

(message "Test completed")
