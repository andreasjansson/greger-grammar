;;; debug-test.el --- Debug greger tree-sitter parsing

(load-file "./greger-tree-sitter.el")

(let ((test-text "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?
"))
  (message "Input text:")
  (message "%s" test-text)

  ;; Parse with tree-sitter to see the tree structure
  (with-temp-buffer
    (insert test-text)
    (let ((parser (treesit-parser-create 'greger)))
      (let ((root-node (treesit-parser-root-node parser)))
        (message "\nTree structure:")
        (message "Root node type: %s" (treesit-node-type root-node))
        (message "Child count: %d" (treesit-node-child-count root-node))
        (dotimes (i (treesit-node-child-count root-node))
          (let ((child (treesit-node-child root-node i)))
            (message "Child %d: %s [%s]" i (treesit-node-type child) (treesit-node-text child)))))))

  (message "\nParsed result:")
  (let ((result (greger-tree-sitter-parse test-text)))
    (pp result)
    (message "\nNumber of messages: %d" (length result))
    (when (> (length result) 0)
      (message "First message role: %s" (alist-get 'role (car result)))
      (message "First message content: %S" (alist-get 'content (car result))))
    (when (> (length result) 1)
      (message "Second message role: %s" (alist-get 'role (cadr result)))
      (message "Second message content: %S" (alist-get 'content (cadr result))))))
