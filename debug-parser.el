;;; debug-parser.el --- Debug tree-sitter parsing

(require 'treesit)
(add-to-list 'treesit-extra-load-path "/Users/andreas/scratch/greger-grammar")

(defun debug-tree-sitter-structure (text)
  "Debug the tree-sitter structure for given text."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (debug-print-node root-node 0))))

(defun debug-print-node (node level)
  "Print node structure recursively."
  (let ((indent (make-string (* level 2) ?\s))
        (node-type (treesit-node-type node))
        (node-text (treesit-node-text node)))
    (message "%s%s: %S" indent node-type
             (if (< (length node-text) 50)
                 node-text
               (concat (substring node-text 0 47) "...")))
    (dolist (child (treesit-node-children node))
      (debug-print-node child (1+ level)))))

;; Test with simple conversation
(let ((test-text "## USER:\n\nHello\n\n## ASSISTANT:\n\nHi there! How can I help you today?"))
  (debug-tree-sitter-structure test-text))
