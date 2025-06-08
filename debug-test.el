(load-file "./greger-tree-sitter.el")

;; Test the tool-use-with-code-in-params case specifically
(let* ((markdown (with-temp-buffer
                   (insert-file-contents "./test/corpus/tool-use-with-code-in-params.greger")
                   (buffer-string)))
       (result (greger-tree-sitter-parse markdown)))
  (message "=== TOOL USE WITH CODE IN PARAMS ===")
  (message "Parse result:")
  (pp result)
  ;; Look specifically at the tool use type
  (let ((assistant-content (alist-get 'content (nth 1 result))))
    (message "\nAssistant content first item type: %s"
             (alist-get 'type (car assistant-content))))
  ;; Look specifically at the tool result type
  (let ((user-content (alist-get 'content (nth 2 result))))
    (message "Tool result type: %s"
             (alist-get 'type (car user-content)))))

(provide 'debug-test)
