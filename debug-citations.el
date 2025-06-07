;;; debug-citations.el --- Debug citation parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun debug-citation-parsing ()
  "Debug citation parsing with a simple case."
  (let ((test-text "## USER:

Test citations

## ASSISTANT:

<cite>This is cited text</cite>

## CITATIONS:

### https://example.com

Title: Example
Cited text: This is cited text
Encrypted index: abc123"))
    (condition-case err
        (let ((result (greger-tree-sitter-parse test-text)))
          (message "Citation parsing result:")
          (pp result))
      (error
       (message "Error: %s" (error-message-string err))))))

(debug-citation-parsing)
