(load-file "./greger-tree-sitter.el")

;; Simple test to see what's happening
(let* ((markdown "## USER:\n\nHello\n\n## TOOL USE:\n\nName: test\nID: test123\n\n### param\n\n<tool.test123>\nvalue\n</tool.test123>\n\n## TOOL RESULT:\n\nID: test123\n\n<tool.test123>\nresult\n</tool.test123>\n\n## ASSISTANT:\n\nDone")
       (result (greger-tree-sitter-parse markdown)))
  (message "=== SIMPLE TOOL USE TEST ===")
  (message "Parse result:")
  (pp result))

(provide 'debug-test)
