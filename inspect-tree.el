#!/usr/bin/env emacs --script
;;; inspect-tree.el --- Inspect tree-sitter parse tree

;; Load the grgfoo mode to get treesit setup
(load-file "grgfoo.el")

(defun print-tree (node &optional indent)
  "Print the tree structure of NODE with INDENT."
  (let ((indent (or indent 0)))
    (message "%s%s: %s"
             (make-string indent ?\ )
             (treesit-node-type node)
             (replace-regexp-in-string "\n" "\\\\n" (treesit-node-text node)))
    (let ((child-count (treesit-node-child-count node)))
      (dotimes (i child-count)
        (print-tree (treesit-node-child node i) (+ indent 2))))))

;; Test parsing
(with-temp-buffer
  (insert "## USER:\nHello world\n")

  (let ((parser (treesit-parser-create 'greger)))
    (message "=== Parse tree structure ===")
    (print-tree (treesit-parser-root-node parser))))

(message "\n=== Parser test completed ===")
