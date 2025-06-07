(load-file "./greger-tree-sitter.el")

(let* ((text "## USER:

Hello, how are you?")
       (parser (treesit-parser-create 'greger))
       (root-node (progn (with-temp-buffer
                          (insert text)
                          (treesit-parser-root-node parser))))
       (sections (greger-tree-sitter--get-all-sections root-node)))

  (message "Sections found: %d" (length sections))
  (when (> (length sections) 0)
    (let* ((first-section (car sections))
           (user-section-node (treesit-node-child first-section 0))
           (content-node (greger-tree-sitter--find-child-by-type user-section-node "section_content")))

      (message "User section type: %s" (treesit-node-type user-section-node))
      (message "Content node found: %s" (if content-node "yes" "no"))
      (when content-node
        (message "Content node type: %s" (treesit-node-type content-node))
        (message "Content node children: %d" (treesit-node-child-count content-node))
        (dotimes (i (treesit-node-child-count content-node))
          (let ((child (treesit-node-child content-node i)))
            (message "  Child %d: %s" i (treesit-node-type child))))
        (message "Extracted content: '%s'" (greger-tree-sitter--extract-content content-node))))))
