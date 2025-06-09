#!/usr/bin/env emacs --script
;;; test-real-scenario.el --- Test the exact user scenario

(setq debug-on-error t)

;; Load the grgfoo mode
(load-file "grgfoo.el")

(message "Testing the exact scenario: create test.grgfoo, type, hit return...")

;; Create the test file
(let ((test-file "test.grgfoo"))
  (condition-case err
      (progn
        ;; Create file and activate mode (simulating opening test.grgfoo)
        (find-file test-file)
        (message "✓ File created and opened")

        ;; The mode should activate automatically due to auto-mode-alist
        (message "Current mode: %s" major-mode)
        (unless (eq major-mode 'grgfoo-mode)
          (grgfoo-mode)
          (message "✓ Mode activated manually"))

        ;; Type some content (simulating user typing)
        (insert "## USER:")
        (message "✓ Text typed: '## USER:'")

        ;; Hit return (this was causing the segfault)
        (newline)
        (message "✓ Return key pressed successfully")

        ;; Type more content
        (insert "Hello, this is a test")
        (message "✓ More text typed")

        ;; Try another newline
        (newline)
        (message "✓ Second newline successful")

        ;; Show the buffer content
        (message "Final buffer content:")
        (message "%s" (buffer-string))

        ;; Check that tree-sitter is working
        (when (treesit-ready-p 'greger)
          (let ((root (treesit-buffer-root-node)))
            (when root
              (message "✓ Tree-sitter parsing: %s" (treesit-node-type root)))))

        ;; Clean up
        (kill-buffer)
        (when (file-exists-p test-file)
          (delete-file test-file))
        (message "✓ Cleaned up")

        (message "\n🎉 SUCCESS: No segfault! The issue has been fixed."))
    (error (message "❌ ERROR: %s" err))))

(message "Test completed")
