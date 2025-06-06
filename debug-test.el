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

        ;; Check section order
        (let ((sections (greger-tree-sitter--get-all-sections root-node)))
          (message "\nExtracted sections:")
          (dotimes (i (length sections))
            (let* ((section (nth i sections))
                   (section-type (greger-tree-sitter--get-section-type section)))
              (message "Section %d: %s" i section-type)))

          ;; Test the section processing manually
          (let ((processed-result (greger-tree-sitter--process-sections-with-citations sections)))
            (message "\nProcessed sections result:")
            (pp processed-result))))))

  (message "\nFinal parsed result:")
  (let ((result (greger-tree-sitter-parse test-text)))
    (pp result)
    (message "\nNumber of messages: %d" (length result))
    (dolist (msg result)
      (message "Message role: %s" (alist-get 'role msg)))))
