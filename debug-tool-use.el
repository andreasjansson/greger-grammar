(load-file "./greger-tree-sitter.el")

(defun debug-tool-use-test ()
  "Debug tool use parsing."
  (let* ((text (with-temp-buffer
                 (insert-file-contents "./test/corpus/tool-use-with-code-in-params.greger")
                 (buffer-string)))
         (result (greger-tree-sitter-parse text)))
    (message "Input text length: %d" (length text))
    (message "Result:")
    (pp result)
    result))

(debug-tool-use-test)
