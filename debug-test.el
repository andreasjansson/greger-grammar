(load-file "./greger-tree-sitter.el")

;; Test the tool-use-with-code-in-params case specifically
(let* ((markdown (with-temp-buffer
                   (insert-file-contents "./test/corpus/tool-use-with-code-in-params.greger")
                   (buffer-string)))
       (result (greger-tree-sitter-parse markdown)))
  (message "=== TOOL USE WITH CODE IN PARAMS ===")
  (message "Markdown content:\n%s" markdown)
  (message "\nParse result:")
  (pp result))

(provide 'debug-test)
