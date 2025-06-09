;;; debug-parser.el --- Debug the greger parser -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun debug-parse (filename)
  "Debug parse a test file."
  (let* ((file-path (format "./test/corpus/%s.txt" filename))
         (content (with-temp-buffer
                    (insert-file-contents file-path)
                    (let ((content (buffer-string)))
                      (if (string-match "=\\{10,\\}\n.*?\n=\\{10,\\}\n\n\\(\\(?:.\\|\n\\)*?\\)\n---" content)
                          (match-string 1 content)
                        (error "Could not parse test file format: %s" file-path))))))
    (message "Content: %s" content)
    (with-temp-buffer
      (insert content)
      (let* ((parser (treesit-parser-create 'greger))
             (root-node (treesit-parser-root-node parser)))
        (message "Root node children count: %d" (length (treesit-node-children root-node)))
        (dolist (child (treesit-node-children root-node))
          (message "Child type: %s" (treesit-node-type child))
          (message "Child text: %s" (treesit-node-text child))
          (message "Child node structure:")
          (greger-tree-sitter--debug-node child 1)
          (let ((entry (greger-tree-sitter--extract-entry-from-node child)))
            (message "Entry: %s" entry)))
        (let ((result (greger-tree-sitter-parse content)))
          (message "Final result: %s" result)
          result)))))

;; Test simple conversation
;(debug-parse "simple-conversation")

;; Test code blocks
(debug-parse "code-block-triple-backticks")
