;;; test-complex-citations.el --- Complex citation parsing test -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-complex-citations ()
  "Test the citations-basic case without JSON strings."
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
Search results found information about Claude Shannon birth date
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."))

    (message "\n=== Testing complex citations ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          (message "✅ Complex citations test completed!")
          (message "\nResult structure:")
          (message "Number of messages: %d" (length result))
          (dolist (msg result)
            (message "- Role: %s" (alist-get 'role msg))
            (let ((content (alist-get 'content msg)))
              (if (stringp content)
                  (message "  Content: %s" (substring content 0 (min 50 (length content))))
                (message "  Content blocks: %d" (length content))
                (dolist (block content)
                  (message "    - Type: %s" (alist-get 'type block))))))
          (message "\nFull result:")
          (pp result)
          result)
      (error
       (message "❌ Error: %s" (error-message-string err))
       nil))))

;; Run test
(if (treesit-ready-p 'greger)
    (test-complex-citations)
  (message "Tree-sitter greger parser not available"))
