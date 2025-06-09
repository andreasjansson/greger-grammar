#!/usr/bin/env emacs --script
;;; test-parser.el --- Test tree-sitter parser directly

;; Load the grgfoo mode to get treesit setup
(load-file "grgfoo.el")

;; Test parsing
(with-temp-buffer
  (insert "## USER:\nHello world\n")

  ;; Check if we can create a parser
  (condition-case err
      (let ((parser (treesit-parser-create 'greger)))
        (message "Parser created successfully")
        (let ((tree (treesit-parser-root-node parser)))
          (message "Root node: %s" tree)
          (message "Root node type: %s" (treesit-node-type tree))
          (message "Root node text: %s" (treesit-node-text tree))

          ;; Print children
          (let ((child (treesit-node-child tree 0)))
            (when child
              (message "First child type: %s" (treesit-node-type child))
              (message "First child text: %s" (treesit-node-text child))))))
    (error (message "Error creating parser: %s" err))))

(message "Parser test completed")
