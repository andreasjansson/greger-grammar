;;; debug-extraction.el --- Debug content extraction -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun debug-tree-structure ()
  "Debug the tree structure for simple user message."
  (let ((text "## USER:

Hello, how are you?"))
    (with-temp-buffer
      (insert text)
      (let ((parser (treesit-parser-create 'greger)))
        (let ((root-node (treesit-parser-root-node parser)))
          (message "\n=== Tree Structure Debug ===")
          (message "Root node type: %s" (treesit-node-type root-node))
          (message "Root node text: %S" (treesit-node-text root-node))

          ;; Check children
          (let ((child-count (treesit-node-child-count root-node)))
            (message "Root has %d children" child-count)
            (dotimes (i child-count)
              (let ((child (treesit-node-child root-node i)))
                (message "Child %d: type=%s text=%S" i (treesit-node-type child) (treesit-node-text child))

                ;; Check grandchildren for section
                (when (equal (treesit-node-type child) "section")
                  (let ((section-child-count (treesit-node-child-count child)))
                    (message "  Section has %d children" section-child-count)
                    (dotimes (j section-child-count)
                      (let ((section-child (treesit-node-child child j)))
                        (message "  Section child %d: type=%s text=%S"
                                j (treesit-node-type section-child) (treesit-node-text section-child))

                        ;; Check content
                        (when (equal (treesit-node-type section-child) "user_section")
                          (let ((user-child-count (treesit-node-child-count section-child)))
                            (message "    User section has %d children" user-child-count)
                            (dotimes (k user-child-count)
                              (let ((user-child (treesit-node-child section-child k)))
                                (message "    User child %d: type=%s text=%S"
                                        k (treesit-node-type user-child) (treesit-node-text user-child))))))))))))))))

;; Run debug
(if (treesit-ready-p 'greger)
    (debug-tree-structure)
  (message "Tree-sitter greger parser not available"))
