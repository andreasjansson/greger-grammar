;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-code-blocks ()
  "Test code block functionality."
  (let ((text "## USER:

Here's some code:

```
## ASSISTANT:
This should not be parsed as a section header
```

What do you think?"))
    (message "Testing code blocks...")
    (condition-case err
        (let ((result (greger-tree-sitter-parse text)))
          (message "Parse result: %S" result)
          (dolist (msg result)
            (message "Message: role=%s content=%S" (alist-get 'role msg) (alist-get 'content msg))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing code blocks ===")
(test-code-blocks)
