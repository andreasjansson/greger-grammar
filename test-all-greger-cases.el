;;; Simple debug test for greger-tree-sitter -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; Simple test
(message "Testing greger-tree-sitter...")

(if (treesit-ready-p 'greger)
    (progn
      (message "✅ Tree-sitter greger parser is available")

      ;; Test simple parsing
      (let* ((test-content "## USER:\n\nHello, how are you?\n\n## ASSISTANT:\n\nI'm doing well, thanks!")
             (result (greger-tree-sitter-parse test-content)))
        (message "Parse result:")
        (pp result)))
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
