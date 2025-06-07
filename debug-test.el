;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-content-extraction ()
  "Test content extraction."
  (let ((text "## USER:

When was Claude Shannon born?"))
    (message "Testing content extraction...")
    (condition-case err
        (with-temp-buffer
          (insert text)
          (let* ((parser (treesit-parser-create 'greger))
                 (root-node (treesit-parser-root-node parser))
                 (section (treesit-node-child root-node 0))
                 (user-section (treesit-node-child section 0))
                 (content-node (treesit-node-child user-section 1)))
            (message "Content node type: %s" (treesit-node-type content-node))
            (message "Content node text: %S" (treesit-node-text content-node))
            (message "Content node children count: %d" (treesit-node-child-count content-node))
            (dotimes (i (treesit-node-child-count content-node))
              (let ((child (treesit-node-child content-node i)))
                (message "  Content child %d: type=%s text=%S" i (treesit-node-type child) (treesit-node-text child))))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing content extraction ===")
(test-content-extraction)
