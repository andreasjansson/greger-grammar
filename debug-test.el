;;; debug-test.el --- Debug greger tree-sitter parsing

(load-file "./greger-tree-sitter.el")

(let ((test-text "## USER:

Hello, how are you?

## ASSISTANT:

Hi there!"))
  (message "Input text:")
  (message "%s" test-text)
  (message "\nParsed result:")
  (let ((result (greger-tree-sitter-parse test-text)))
    (pp result)
    (message "\nFirst message role: %s" (alist-get 'role (car result)))
    (message "First message content: %S" (alist-get 'content (car result)))))
