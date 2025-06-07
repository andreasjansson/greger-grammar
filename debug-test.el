;;; debug-test.el --- Debug greger-tree-sitter parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-citation-parsing ()
  "Test citation parsing functionality."
  (let ((text "## USER:

When was Claude Shannon born?

## ASSISTANT:

<cite>Claude Shannon was born on April 30, 1916</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001)
Encrypted index: abc123"))
    (message "Testing citation parsing...")
    (condition-case err
        (let ((result (greger-tree-sitter-parse text)))
          (message "Parse result: %S" result)
          (dolist (msg result)
            (message "Message: role=%s content=%S" (alist-get 'role msg) (alist-get 'content msg))))
      (error
       (message "ERROR: %S" err)))))

(message "=== Testing citation parsing ===")
(test-citation-parsing)
