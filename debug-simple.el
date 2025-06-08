(load-file "./greger-tree-sitter.el")

;; Test with simple user message
(let ((markdown "## USER:

Hello, how are you?"))
  (condition-case err
      (let ((result (greger-tree-sitter-parse markdown)))
        (message "Result: %S" result))
    (error
     (message "Error: %s" (error-message-string err)))))
