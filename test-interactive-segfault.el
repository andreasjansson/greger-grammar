#!/usr/bin/env emacs --script
;;; test-interactive-segfault.el --- More comprehensive segfault test

;; Load debugging aids
(setq debug-on-error t)
(setq debug-on-quit t)

;; Load the grgfoo mode
(message "Loading grgfoo.el...")
(condition-case err
    (load-file "grgfoo.el")
  (error (message "Error loading grgfoo.el: %s" err)))

;; Helper function to test various operations safely
(defun test-operation (name operation-fn)
  "Test OPERATION-FN with NAME, catching errors."
  (message "Testing %s..." name)
  (condition-case err
      (progn
        (funcall operation-fn)
        (message "%s: SUCCESS" name))
    (error (message "%s: ERROR - %s" name err))))

;; Test tree-sitter grammar loading specifically
(test-operation "Grammar loading"
  (lambda ()
    (when (treesit-ready-p 'greger)
      (let ((parser (treesit-parser-create 'greger)))
        (treesit-parser-delete parser)
        (message "Parser created and deleted successfully")))))

;; Test with various buffer operations
(let ((test-file "test-interactive.grgfoo"))
  (test-operation "File creation and mode activation"
    (lambda ()
      (find-file test-file)
      (grgfoo-mode)
      (message "Mode: %s" major-mode)))

  (test-operation "Text insertion"
    (lambda ()
      (insert "## USER:")
      (message "Text inserted")))

  (test-operation "Newline insertion"
    (lambda ()
      (newline)
      (message "Newline inserted")))

  (test-operation "Typing characters"
    (lambda ()
      (insert "Hello world")
      (message "Characters inserted")))

  (test-operation "Another newline"
    (lambda ()
      (newline)
      (message "Second newline inserted")))

  (test-operation "Tree-sitter operations"
    (lambda ()
      (when (treesit-ready-p 'greger)
        (let ((parser (treesit-buffer-root-node)))
          (when parser
            (message "Root node type: %s" (treesit-node-type parser))
            (message "Root node text: %s" (treesit-node-text parser)))))))

  (test-operation "Font lock operations"
    (lambda ()
      (font-lock-ensure)
      (message "Font lock applied")))

  (test-operation "Indentation operations"
    (lambda ()
      (indent-region (point-min) (point-max))
      (message "Indentation applied")))

  (message "Final buffer content:")
  (message "%s" (buffer-string))

  (test-operation "Buffer cleanup"
    (lambda ()
      (kill-buffer)
      (when (file-exists-p test-file)
        (delete-file test-file))
      (message "Cleaned up"))))

(message "All tests completed")
