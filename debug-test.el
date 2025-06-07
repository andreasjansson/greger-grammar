;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-tree-structure ()
  "Test tree structure parsing."
  (let ((text "## USER:

When was Claude Shannon born?"))
    (message "Testing tree structure...")
    (condition-case err
        (with-temp-buffer
          (insert text)
          (let* ((parser (treesit-parser-create 'greger))
                 (root-node (treesit-parser-root-node parser)))
            (message "Root node type: %s" (treesit-node-type root-node))
            (message "Root node children count: %d" (treesit-node-child-count root-node))
            (dotimes (i (treesit-node-child-count root-node))
              (let ((child (treesit-node-child root-node i)))
                (message "  Root child %d: type=%s" i (treesit-node-type child))
                (when (equal (treesit-node-type child) "section")
                  (let* ((section-child (treesit-node-child child 0))
                         (section-type (treesit-node-type section-child)))
                    (message "    Section type: %s" section-type)
                    (message "    Section children count: %d" (treesit-node-child-count section-child))
                    (dotimes (j (treesit-node-child-count section-child))
                      (let ((section-subchild (treesit-node-child section-child j)))
                        (message "      Section child %d: type=%s" j (treesit-node-type section-subchild))))))))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing tree structure ===")
(test-tree-structure)
