#!/usr/bin/env emacs --script
;;; test-node-types.el --- Check what node types exist in parsed content

(setq debug-on-error t)
(load-file "grgfoo.el")

(defun print-node-tree (node depth)
  "Print NODE and its children with DEPTH indentation."
  (let ((indent (make-string (* depth 2) ?\s)))
    (message "%s%s: %S" indent (treesit-node-type node) (treesit-node-text node)))
  (let ((child-count (treesit-node-child-count node)))
    (dotimes (i child-count)
      (let ((child (treesit-node-child node i)))
        (when child
          (print-node-tree child (1+ depth)))))))

(when (treesit-ready-p 'greger)
  (message "Testing different greger content...")

  ;; Test 1: Simple user content
  (message "\n=== Test 1: Simple user ===")
  (with-temp-buffer
    (insert "## USER:\nHello")
    (let* ((parser (treesit-parser-create 'greger))
           (root (treesit-parser-root-node parser)))
      (print-node-tree root 0)))

  ;; Test 2: Tool use
  (message "\n=== Test 2: Tool use ===")
  (with-temp-buffer
    (insert "## TOOL USE:\n\nName: test\nID: 123\n")
    (let* ((parser (treesit-parser-create 'greger))
           (root (treesit-parser-root-node parser)))
      (print-node-tree root 0)))

  ;; Test 3: Citations
  (message "\n=== Test 3: Citations ===")
  (with-temp-buffer
    (insert "## CITATIONS:\n\n### https://example.com\nTitle: Example\n")
    (let* ((parser (treesit-parser-create 'greger))
           (root (treesit-parser-root-node parser)))
      (print-node-tree root 0))))

(message "\nNode type analysis completed")
