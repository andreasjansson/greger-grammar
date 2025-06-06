;;; test-simple-citations.el --- Simple citation parsing test -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-simple-citations ()
  "Test a simple citation case."
  (let ((markdown "## USER:

Hello

## ASSISTANT:

<cite>Some cited text</cite>

## CITATIONS:

### https://example.com

Title: Example
Cited text: This is the full text
Encrypted index: abc123"))
    (message "\n=== Testing simple citations ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          (message "✅ Simple citations test completed!")
          (message "\nResult:")
          (pp result)
          result)
      (error
       (message "❌ Error: %s" (error-message-string err))
       nil))))

;; Run test
(if (treesit-ready-p 'greger)
    (test-simple-citations)
  (message "Tree-sitter greger parser not available"))
