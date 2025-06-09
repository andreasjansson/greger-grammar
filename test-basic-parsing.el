;;; Test basic parsing

(load-file "./greger-tree-sitter.el")

(defun test-simple-case ()
  "Test a very simple case."
  (let ((simple-text "## USER:\n\nHello"))
    (message "Testing simple case...")
    (condition-case err
        (let ((result (greger-tree-sitter-parse simple-text)))
          (message "Success: %S" result))
      (error (message "Error: %S" err)))))

(test-simple-case)
