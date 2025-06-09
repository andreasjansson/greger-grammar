#!/usr/bin/env emacs --script
;;; test-minimal-segfault.el --- Minimal reproduction of segfault

(setq debug-on-error t)
(load-file "grgfoo.el")

(message "Testing minimal segfault scenario...")

(with-temp-buffer
  (condition-case err
      (progn
        ;; Simulate what happens when we open a .grgfoo file
        (grgfoo-mode)
        (message "✓ grgfoo-mode activated")

        ;; Type the exact content that caused issues
        (insert "## USER:")
        (message "✓ Inserted '## USER:'")

        ;; The problematic newline insertion
        (message "About to insert newline...")
        (insert "\n")
        (message "✓ Newline inserted successfully")

        ;; Try a few more operations that might trigger the segfault
        (insert "Hello")
        (message "✓ More text inserted")

        ;; Try syntax highlighting update
        (font-lock-ensure)
        (message "✓ Font lock updated")

        ;; Check the parse tree
        (when (treesit-ready-p 'greger)
          (let ((root (treesit-buffer-root-node)))
            (when root
              (message "✓ Parse tree: %s" (treesit-node-type root)))))

        (message "SUCCESS: No segfault occurred!"))
    (error (message "ERROR: %s" err))))

(message "Test completed")
