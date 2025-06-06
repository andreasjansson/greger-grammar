;;; test-citations.el --- Test citation parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-citations-basic ()
  "Test the citations-basic case."
  (let ((markdown "## USER:

When was Claude Shannon born?

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."))

    (message "\n=== Testing citations-basic ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          (message "Parsed result: %S" result)
          (message "✅ Citations test completed")
          result)
      (error
       (message "❌ Error: %s" err)
       nil))))

;; Run test
(if (treesit-ready-p 'greger)
    (test-citations-basic)
  (message "Tree-sitter greger parser not available"))
