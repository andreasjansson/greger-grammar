;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-thinking ()
  "Test thinking functionality."
  (let ((text "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question. I can answer this directly.

## ASSISTANT:

2 + 2 = 4"))
    (message "Testing thinking...")
    (condition-case err
        (let ((result (greger-tree-sitter-parse text)))
          (message "Parse result: %S" result)
          (dolist (msg result)
            (message "Message: role=%s content=%S" (alist-get 'role msg) (alist-get 'content msg))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing thinking ===")
(test-thinking)
