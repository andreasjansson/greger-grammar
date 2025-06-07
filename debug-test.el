;;; debug-test.el --- Debug tree structure inspection -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun debug-tree-structure ()
  "Debug the tree structure to see what's happening with content."
  (let ((markdown "## USER:

Hello, how are you?"))
    (message "\n=== Debugging tree structure ===")

    (with-temp-buffer
      (insert markdown)
      (let ((parser (treesit-parser-create 'greger)))
        (let* ((root-node (treesit-parser-root-node parser))
               (sections (greger-tree-sitter--get-all-sections root-node)))

          (message "Number of sections: %d" (length sections))

          (when (> (length sections) 0)
            (let* ((first-section (car sections))
                   (section-type (greger-tree-sitter--get-section-type first-section))
                   (user-section (treesit-node-child first-section 0))
                   (content-node (greger-tree-sitter--find-child-by-type user-section "section_content")))

              (message "Section type: %s" section-type)
              (message "User section node: %s" (treesit-node-type user-section))
              (message "Content node: %s" (if content-node (treesit-node-type content-node) "nil"))

              (when content-node
                (message "Content node text (raw): %S" (treesit-node-text content-node))
                (message "Content node child count: %d" (treesit-node-child-count content-node))

                (dotimes (i (treesit-node-child-count content-node))
                  (let ((child (treesit-node-child content-node i)))
                    (message "  Child %d: type=%s text=%S"
                             i
                             (treesit-node-type child)
                             (treesit-node-text child))

                    (when (equal (treesit-node-type child) "content_line")
                      (message "    content_line children:")
                      (dotimes (j (treesit-node-child-count child))
                        (let ((line-child (treesit-node-child child j)))
                          (message "      Child %d: type=%s text=%S"
                                   j
                                   (treesit-node-type line-child)
                                   (treesit-node-text line-child))))))))))))))

;; Run debug
(if (treesit-ready-p 'greger)
    (debug-tree-structure)
  (message "Tree-sitter greger parser not available"))
