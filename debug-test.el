;;; debug-test.el --- Debug greger tree-sitter parsing

(load-file "./greger-tree-sitter.el")

;; Debug simple tree structure first
(let ((text "## USER:

Hello, how are you?"))
  (with-temp-buffer
    (insert text)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))

      (message "Root node type: %s" (treesit-node-type root-node))
      (message "Root node children: %d" (treesit-node-child-count root-node))
      (dotimes (i (treesit-node-child-count root-node))
        (let ((child (treesit-node-child root-node i)))
          (message "  Root child %d: %s" i (treesit-node-type child))))

      (let ((sections (greger-tree-sitter--get-all-sections root-node)))
        (message "Sections found by get-all-sections: %d" (length sections))))))
