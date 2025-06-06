;;; test-citations.el --- Test citation parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-citations-basic ()
  "Test the citations-basic case."
  (let ((markdown "## USER:

When was Claude Shannon born?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

### query

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
claude shannon birth date
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## SERVER TOOL RESULT:

ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",
    \"title\": \"Claude Shannon - Wikipedia\",
    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",
    \"page_age\": \"April 30, 2025\"
  }
]
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."))

    (message "\n=== Testing citations-basic ===")
    (condition-case err
        (progn
          (let ((result (greger-tree-sitter-parse markdown)))
            (message "✅ Citations test completed successfully!")
            (message "\nStructure summary:")
            (message "- User message: %S" (alist-get 'content (cadr result)))
            (let ((assistant-content (alist-get 'content (car result))))
              (message "- Assistant content blocks: %d" (length assistant-content))
              (dotimes (i (length assistant-content))
                (let ((block (nth i assistant-content)))
                  (message "  %d. %s" (1+ i) (alist-get 'type block)))))
            result))
      (error
       (message "❌ Error: %s" (error-message-string err))
       nil))))

;; Run test
(if (treesit-ready-p 'greger)
    (test-citations-basic)
  (message "Tree-sitter greger parser not available"))
