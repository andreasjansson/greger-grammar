(load-file "./greger-tree-sitter.el")

(let* ((text (with-temp-buffer
               (insert-file-contents "./test/corpus/citations-after-tool-result.greger")
               (buffer-string)))
       (result (greger-tree-sitter-parse text)))
  (message "Result:")
  (pp result))
