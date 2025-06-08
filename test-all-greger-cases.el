;;; Simple debug test for greger-tree-sitter -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; Simple test
(message "Testing greger-tree-sitter...")

(if (treesit-ready-p 'greger)
    (progn
      (message "✅ Tree-sitter greger parser is available")

      ;; Test simple parsing
      (let* ((test-content (with-temp-buffer
                             (insert-file-contents "test/corpus/simple-user-message.greger")
                             (buffer-string)))
             (result (greger-tree-sitter-parse test-content)))
        (message "Parse result:")
        (pp result)

        ;; Debug: show raw text extraction
        (with-temp-buffer
          (insert test-content)
          (let* ((parser (treesit-parser-create 'greger))
                 (root-node (treesit-parser-root-node parser))
                 (sections (treesit-node-children root-node)))
            (dolist (section sections)
              (let ((section-type (treesit-node-type section)))
                (message "Section type: %s" section-type)
                (let ((children (treesit-node-children section)))
                  (dolist (child children)
                    (message "  Child type: %s, text: %S"
                             (treesit-node-type child)
                             (treesit-node-text child))))))))))
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
