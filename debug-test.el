;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-basic-parsing ()
  "Test basic parsing functionality."
  (let ((text "## USER:

When was Claude Shannon born?

## ASSISTANT:

He was born in 1916."))
    (message "Testing basic parsing...")
    (condition-case err
        (let ((result (greger-tree-sitter-parse text)))
          (message "Parse result: %S" result)
          (dolist (msg result)
            (message "Message: role=%s content=%S" (alist-get 'role msg) (alist-get 'content msg))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing basic parsing ===")
(test-basic-parsing)
