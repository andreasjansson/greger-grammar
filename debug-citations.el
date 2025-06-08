(load-file "./greger-tree-sitter.el")

(defun debug-citations-test ()
  "Debug citations parsing."
  (let* ((text (with-temp-buffer
                 (insert-file-contents "./test/corpus/citations-basic.greger")
                 (buffer-string)))
         (result (greger-tree-sitter-parse text)))
    (message "Input text length: %d" (length text))
    (message "Result:")
    (pp result)
    result))

(debug-citations-test)
