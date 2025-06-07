;;; debug-citations-simple.el --- Debug simple citation case -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; Override the citation section extraction to add debug output
(defun greger-tree-sitter--extract-citations-section (section-node)
  "Debug version that prints what it finds."
  (message "DEBUG: extract-citations-section called")
  (let* ((citations-section (treesit-node-child section-node 0))
         (citations-content (greger-tree-sitter--find-child-by-type citations-section "citations_content"))
         (citations '()))

    (message "DEBUG: citations-section = %s" (treesit-node-type citations-section))
    (message "DEBUG: citations-content = %s" (if citations-content (treesit-node-type citations-content) "nil"))

    (when citations-content
      (let ((child-count (treesit-node-child-count citations-content)))
        (message "DEBUG: citations-content has %d children" child-count)
        (dotimes (i child-count)
          (let ((child (treesit-node-child citations-content i)))
            (message "DEBUG: child %d type = %s" i (treesit-node-type child))
            (when (equal (treesit-node-type child) "citation_entry")
              (message "DEBUG: Found citation_entry")
              (push (greger-tree-sitter--extract-citation-entry child) citations))))))

    (message "DEBUG: extracted %d citations" (length citations))
    (nreverse citations)))

;; Test with simple case
(let ((test-text "## USER:

Test

## ASSISTANT:

<cite>cited text</cite>

## CITATIONS:

### https://example.com

Title: Example
Cited text: cited text
Encrypted index: abc123"))
  (message "\n=== Testing citation detection ===")
  (condition-case err
      (progn
        (message "Input text: %s" test-text)
        (let ((result (greger-tree-sitter-parse test-text)))
          (message "Result:")
          (pp result)))
    (error
     (message "Error: %s" (error-message-string err)))))
