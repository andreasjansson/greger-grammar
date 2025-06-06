;;; debug-test.el --- Debug greger tree-sitter parsing

(load-file "./greger-tree-sitter.el")

(let ((test-text "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?"))
  (message "Input text:")
  (message "%s" test-text)
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
