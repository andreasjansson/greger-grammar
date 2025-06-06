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
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")
        (expected '(((role . "user") (content . "When was Claude Shannon born?"))
                    ((role . "assistant")
                     (content . (((type . "server_tool_use")
                                  (id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (name . "web_search")
                                  (input . ((query . "claude shannon birth date"))))
                                 ((type . "web_search_tool_result")
                                  (tool_use_id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (content . (((type . "web_search_result")
                                               (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                               (title . "Claude Shannon - Wikipedia")
                                               (encrypted_content . "EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...")
                                               (page_age . "April 30, 2025")))))
                                 ((type . "text")
                                  (text . "Based on the search results, "))
                                 ((type . "text")
                                  (text . "Claude Shannon was born on April 30, 1916, in Petoskey, Michigan")
                                  (citations . (((type . "web_search_result_location")
                                                 (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                 (title . "Claude Shannon - Wikipedia")
                                                 (cited_text . "Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...")
                                                 (encrypted_index . "Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")))))))))

    (message "\n=== Testing citations-basic ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          (if (equal result expected)
              (message "✅ Citations test PASSED!")
            (progn
              (message "❌ Citations test FAILED!")
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp result)))
          result)
      (error
       (message "❌ Error: %s" (error-message-string err))
       nil))))

;; Run test
(if (treesit-ready-p 'greger)
    (test-citations-basic)
  (message "Tree-sitter greger parser not available"))
