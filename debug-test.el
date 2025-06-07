;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-basic-citation ()
  "Test basic citation parsing."
  (let ((text "## USER:

When was Claude Shannon born?

## ASSISTANT:

<cite>Claude Shannon was born on April 30, 1916</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001)
Encrypted index: abc123"))
    (message "Testing basic citation...")
    (condition-case err
        (let ((result (greger-tree-sitter-parse text)))
          (message "Parse result: %S" result)
          (message "First message: %S" (car result))
          (message "Second message: %S" (cadr result))
          (when (cdr result)
            (message "Second message content: %S" (alist-get 'content (cadr result)))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing basic citation ===")
(test-basic-citation)
