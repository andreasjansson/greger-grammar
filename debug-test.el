;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-tree-structure ()
  "Test tree structure parsing."
  (let ((text "## USER:

When was Claude Shannon born?"))
    (message "Testing tree structure...")
    (condition-case err
        (let ((parser (treesit-parser-create 'greger)))
          (with-temp-buffer
            (insert text)
            (let* ((root-node (treesit-parser-root-node parser))
                   (sections (greger-tree-sitter--get-all-sections root-node)))
              (message "Root node type: %s" (treesit-node-type root-node))
              (message "Number of sections: %d" (length sections))
              (when sections
                (let* ((first-section (car sections))
                       (section-type (treesit-node-type (treesit-node-child first-section 0)))
                       (user-section (treesit-node-child first-section 0))
                       (content-node (greger-tree-sitter--find-child-by-type user-section "content")))
                  (message "First section type: %s" section-type)
                  (message "User section children count: %d" (treesit-node-child-count user-section))
                  (dotimes (i (treesit-node-child-count user-section))
                    (let ((child (treesit-node-child user-section i)))
                      (message "  Child %d: type=%s text=%S" i (treesit-node-type child) (treesit-node-text child))))
                  (when content-node
                    (message "Content node type: %s" (treesit-node-type content-node))
                    (message "Content node children count: %d" (treesit-node-child-count content-node))
                    (dotimes (i (treesit-node-child-count content-node))
                      (let ((child (treesit-node-child content-node i)))
                        (message "  Content child %d: type=%s text=%S" i (treesit-node-type child) (treesit-node-text child))))))))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing tree structure ===")
(test-tree-structure)
