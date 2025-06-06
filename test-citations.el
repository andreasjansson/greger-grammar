;;; test-citations.el --- Test citation parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-citations-basic ()
  "Test the citations-basic case."
  (let ((markdown (concat "## USER:\n\n"
                          "When was Claude Shannon born?\n\n"
                          "## SERVER TOOL USE:\n\n"
                          "Name: web_search\n"
                          "ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE\n\n"
                          "### query\n\n"
                          "<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>\n"
                          "claude shannon birth date\n"
                          "</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>\n\n"
                          "## SERVER TOOL RESULT:\n\n"
                          "ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE\n\n"
                          "<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>\n"
                          "Some search result content about Claude Shannon\n"
                          "</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>\n\n"
                          "## ASSISTANT:\n\n"
                          "Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>\n\n"
                          "## CITATIONS:\n\n"
                          "### https://en.wikipedia.org/wiki/Claude_Shannon\n\n"
                          "Title: Claude Shannon - Wikipedia\n"
                          "Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...\n"
                          "Encrypted index: Eo8BCioIAhgBIiQyYjQ4OWJmZi1lNm.."))
        (expected '(((role . "user") (content . "When was Claude Shannon born?"))
                    ((role . "assistant")
                     (content . (((type . "server_tool_use")
                                  (id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (name . "web_search")
                                  (input . ((query . "claude shannon birth date"))))
                                 ((type . "web_search_tool_result")
                                  (tool_use_id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (content . "Some search result content about Claude Shannon"))
                                 ((type . "text")
                                  (text . "Based on the search results, "))
                                 ((type . "text")
                                  (text . "Claude Shannon was born on April 30, 1916, in Petoskey, Michigan")
                                  (citations . (((type . "web_search_result_location")
                                                 (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                 (title . "Claude Shannon - Wikipedia")
                                                 (cited_text . "Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...")
                                                 (encrypted_index . "Eo8BCioIAhgBIiQyYjQ4OWJmZi1lNm..")))))))))))

    (message "\n=== Testing citations-basic ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          ;; Reverse the order to match expected (user first, then assistant)
          (setq result (reverse result))
          (if (equal result expected)
              (message "✅ Citations test PASSED!")
            (progn
              (message "❌ Citations test FAILED!")
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp result)
              (message "\nComparison:")
              (message "- Expected messages: %d" (length expected))
              (message "- Actual messages: %d" (length result))
              (when (> (length result) 0)
                (message "- First message role: %s (expected: user)"
                         (alist-get 'role (car result))))
              (when (> (length result) 1)
                (let ((asst-content (alist-get 'content (cadr result))))
                  (message "- Assistant content blocks: %d (expected: 4)"
                           (if (listp asst-content) (length asst-content) 0))))))
          result)
      (error
       (message "❌ Error: %s" (error-message-string err))
       nil))))

;; Run test
(if (treesit-ready-p 'greger)
    (test-citations-basic)
  (message "Tree-sitter greger parser not available"))
