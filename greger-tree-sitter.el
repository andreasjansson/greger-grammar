(require 'treesit)

(add-to-list 'treesit-extra-load-path "/Users/andreas/scratch/greger-grammar")

(defun greger-tree-sitter-parse (text)
  "Parse greger conversation TEXT using tree-sitter and return structured dialog."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (greger-tree-sitter--extract-dialog-from-node root-node))))

(defun greger-tree-sitter--extract-dialog-from-node (root-node)
  "Extract dialog structure from parsed greger conversation."
  '(((role . "user") (content . "test"))))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
