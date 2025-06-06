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
Some search result content about Claude Shannon
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ4OWJmZi1lNm..")
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
                                  (text . "Based on the search results,"))
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
              (message "✅ Citations-basic test PASSED!")
            (progn
              (message "❌ Citations-basic test FAILED!")
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp result)))
          result)
      (error
       (message "❌ Citations-basic Error: %s" (error-message-string err))
       nil))))

(defun test-citations-after-tool-result ()
  "Test citations immediately after tool result."
  (let ((markdown "## USER:

What's the current weather?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_456

### query

<tool.srvtoolu_456>
current weather
</tool.srvtoolu_456>

## SERVER TOOL RESULT:

ID: srvtoolu_456

<tool.srvtoolu_456>
Search results about current weather
</tool.srvtoolu_456>

## ASSISTANT:

<cite>It's currently sunny and 75°F</cite>

## CITATIONS:

### https://weather.com

Title: Weather.com
Cited text: Currently sunny with a temperature of 75 degrees Fahrenheit...
Encrypted index: xyz789")
        (expected '(((role . "user") (content . "What's the current weather?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_456")
                                                       (name . "web_search")
                                                       (input . ((query . "current weather"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_456")
                                                       (content . "Search results about current weather"))
                                                      ((type . "text")
                                                       (text . "It's currently sunny and 75°F")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://weather.com")
                                                                      (title . "Weather.com")
                                                                      (cited_text . "Currently sunny with a temperature of 75 degrees Fahrenheit...")
                                                                      (encrypted_index . "xyz789")))))))))))

    (message "\n=== Testing citations-after-tool-result ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          ;; Reverse the order to match expected (user first, then assistant)
          (setq result (reverse result))
          (if (equal result expected)
              (message "✅ Citations-after-tool-result test PASSED!")
            (progn
              (message "❌ Citations-after-tool-result test FAILED!")
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp result)))
          result)
      (error
       (message "❌ Citations-after-tool-result Error: %s" (error-message-string err))
       nil))))

(defun test-citations-multiple ()
  "Test multiple citations in same text."
  (let ((markdown "## USER:

Tell me about Einstein and Newton

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_789

### query

<tool.srvtoolu_789>
Einstein Newton physics
</tool.srvtoolu_789>

## SERVER TOOL RESULT:

ID: srvtoolu_789

<tool.srvtoolu_789>
Search results about Einstein and Newton
</tool.srvtoolu_789>

## ASSISTANT:

<cite>Einstein developed the theory of relativity</cite>

## CITATIONS:

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

## ASSISTANT:

while <cite>Newton formulated the laws of motion</cite>

## CITATIONS:

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789")
        (expected '(((role . "user") (content . "Tell me about Einstein and Newton"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_789")
                                                       (name . "web_search")
                                                       (input . ((query . "Einstein Newton physics"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_789")
                                                       (content . "Search results about Einstein and Newton"))
                                                      ((type . "text")
                                                       (text . "Einstein developed the theory of relativity")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://physics.com/einstein")
                                                                      (title . "Einstein Biography")
                                                                      (cited_text . "Albert Einstein developed the theory of relativity in the early 20th century...")
                                                                      (encrypted_index . "def456")))))
                                                      ((type . "text") (text . "while"))
                                                      ((type . "text")
                                                       (text . "Newton formulated the laws of motion")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://physics.com/newton")
                                                                      (title . "Newton Biography")
                                                                      (cited_text . "Isaac Newton formulated the three laws of motion...")
                                                                      (encrypted_index . "ghi789")))))))))))

    (message "\n=== Testing citations-multiple ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          ;; Reverse the order to match expected (user first, then assistant)
          (setq result (reverse result))
          (if (equal result expected)
              (message "✅ Citations-multiple test PASSED!")
            (progn
              (message "❌ Citations-multiple test FAILED!")
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp result)))
          result)
      (error
       (message "❌ Citations-multiple Error: %s" (error-message-string err))
       nil))))

;; Run all tests
(if (treesit-ready-p 'greger)
    (progn
      (test-citations-basic)
      (test-citations-after-tool-result)
      (test-citations-multiple)
      (message "\n=== All citation tests completed ==="))
  (message "Tree-sitter greger parser not available"))
