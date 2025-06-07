;;; test-all-greger-cases.el --- Comprehensive test of greger-tree-sitter against all test cases -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; All test cases from greger-parser-test-cases
(defconst greger-tree-sitter-test-cases
  '(
    ;; Simple user message
    (:name "simple-user-message"
           :markdown "## USER:

Hello, how are you?"
           :dialog (((role . "user") (content . "Hello, how are you?"))))

    ;; System and user message
    (:name "system-and-user"
           :markdown "## SYSTEM:

You are a helpful assistant.

## USER:

What's the weather like?"
           :dialog (((role . "system") (content . "You are a helpful assistant."))
                    ((role . "user") (content . "What's the weather like?"))))

    ;; Simple conversation
    (:name "simple-conversation"
           :markdown "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?"
           :dialog (((role . "user") (content . "Hello"))
                    ((role . "assistant") (content . "Hi there! How can I help you today?"))))

    ;; Thinking section (becomes part of assistant message)
    (:name "thinking-section"
           :markdown "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question. I can answer this directly without needing any tools.

## ASSISTANT:

2 + 2 = 4"
           :dialog (((role . "user") (content . "What's 2+2?"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "This is a simple arithmetic question. I can answer this directly without needing any tools.")) ((type . "text") (text . "2 + 2 = 4")))))))

    ;; Tool use with single parameter
    (:name "tool-use-single-param"
           :markdown "## USER:

Read the file hello.txt

## TOOL USE:

Name: read-file
ID: toolu_123

### path

<tool.toolu_123>
hello.txt
</tool.toolu_123>

## TOOL RESULT:

ID: toolu_123

<tool.toolu_123>
Hello, world!
</tool.toolu_123>

## ASSISTANT:

The file contains: Hello, world!"
           :dialog (((role . "user") (content . "Read the file hello.txt"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_123") (name . "read-file") (input . ((path . "hello.txt")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_123") (content . "Hello, world!")))))
                    ((role . "assistant") (content . "The file contains: Hello, world!"))))

    ;; Tool use with multiple parameters
    (:name "tool-use-multiple-params"
           :markdown "## USER:

Search for python files containing 'def main'

## TOOL USE:

Name: ripgrep
ID: toolu_456

### pattern

<tool.toolu_456>
def main
</tool.toolu_456>

### file-type

<tool.toolu_456>
py
</tool.toolu_456>

### context-lines

<tool.toolu_456>
2
</tool.toolu_456>

## TOOL RESULT:

ID: toolu_456

<tool.toolu_456>
src/main.py:10:def main():
src/utils.py:25:def main_helper():
</tool.toolu_456>

## ASSISTANT:

I found 2 matches for 'def main' in Python files."
           :dialog (((role . "user") (content . "Search for python files containing 'def main'"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_456") (name . "ripgrep") (input . ((pattern . "def main") (file-type . "py") (context-lines . 2)))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_456") (content . "src/main.py:10:def main():\nsrc/utils.py:25:def main_helper():")))))
                    ((role . "assistant") (content . "I found 2 matches for 'def main' in Python files."))))

    ;; Complex workflow with thinking, tool use, and multiple responses
    (:name "complex-workflow"
           :markdown "## USER:

who's the current king of sweden?

## THINKING:

The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information.

## TOOL USE:

Name: search-286d2fd3
ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

### query

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
current king of Sweden 2024
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

### include_answer

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
basic
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

### max_results

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
3
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

## TOOL RESULT:

ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
JSON result content
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

## ASSISTANT:

The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."
           :dialog (((role . "user") (content . "who's the current king of sweden?"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information.")) ((type . "tool_use") (id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc") (name . "search-286d2fd3") (input . ((query . "current king of Sweden 2024") (include_answer . "basic") (max_results . 3)))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc") (content . "JSON result content")))))
                    ((role . "assistant") (content . "The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."))))

    ;; Multiple tool uses in sequence
    (:name "multiple-tool-uses"
           :markdown "## USER:

List files and read the first one

## TOOL USE:

Name: list-directory
ID: toolu_111

### path

<tool.toolu_111>
.
</tool.toolu_111>

## TOOL RESULT:

ID: toolu_111

<tool.toolu_111>
file1.txt
file2.txt
README.md
</tool.toolu_111>

## TOOL USE:

Name: read-file
ID: toolu_222

### path

<tool.toolu_222>
file1.txt
</tool.toolu_222>

## TOOL RESULT:

ID: toolu_222

<tool.toolu_222>
This is the content of file1.
</tool.toolu_222>

## ASSISTANT:

I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\""
           :dialog (((role . "user") (content . "List files and read the first one"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_111") (name . "list-directory") (input . ((path . ".")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_111") (content . "file1.txt\nfile2.txt\nREADME.md")))))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_222") (name . "read-file") (input . ((path . "file1.txt")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_222") (content . "This is the content of file1.")))))
                    ((role . "assistant") (content . "I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\""))))

    ;; Tool use with multiline parameter values
    (:name "tool-use-multiline-params"
           :markdown "## USER:

Write a new Python file

## TOOL USE:

Name: write-new-file
ID: toolu_789

### file_path

<tool.toolu_789>
script.py
</tool.toolu_789>

### contents

<tool.toolu_789>
#!/usr/bin/env python3

def main():
    print(\"Hello, world!\")

if __name__ == \"__main__\":
    main()
</tool.toolu_789>

### git_commit_message

<tool.toolu_789>
Add new Python script
</tool.toolu_789>

## TOOL RESULT:

ID: toolu_789

<tool.toolu_789>
Successfully wrote new file script.py with 85 characters.
</tool.toolu_789>

## ASSISTANT:

I've created a new Python script file with a basic Hello World program."
           :dialog (((role . "user") (content . "Write a new Python file"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_789") (name . "write-new-file") (input . ((file_path . "script.py") (contents . "#!/usr/bin/env python3\n\ndef main():\n    print(\"Hello, world!\")\n\nif __name__ == \"__main__\":\n    main()") (git_commit_message . "Add new Python script")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_789") (content . "Successfully wrote new file script.py with 85 characters.")))))
                    ((role . "assistant") (content . "I've created a new Python script file with a basic Hello World program."))))

    ;; Just thinking without any other content
    (:name "thinking-only"
           :markdown "## USER:

Let me think about this

## THINKING:

I need to consider all the options carefully before responding."
           :dialog (((role . "user") (content . "Let me think about this"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "I need to consider all the options carefully before responding.")))))))

    ;; Tool use without any following content
    (:name "tool-use-only"
           :markdown "## USER:

Read a file

## TOOL USE:

Name: read-file
ID: toolu_999

### path

<tool.toolu_999>
test.txt
</tool.toolu_999>
"
           :dialog (((role . "user") (content . "Read a file"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_999") (name . "read-file") (input . ((path . "test.txt")))))))))

    ;; Citations basic test
    (:name "citations-basic"
           :markdown "## USER:

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
Search results about Claude Shannon
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."
           :dialog (((role . "user") (content . "When was Claude Shannon born?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                                       (name . "web_search")
                                                       (input . ((query . "claude shannon birth date"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                                       (content . "Search results about Claude Shannon"))
                                                      ((type . "text")
                                                       (text . "Based on the search results,"))
                                                      ((type . "text")
                                                       (text . "Claude Shannon was born on April 30, 1916, in Petoskey, Michigan")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                                      (title . "Claude Shannon - Wikipedia")
                                                                      (cited_text . "Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...")
                                                                      (encrypted_index . "Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."))))))))))

    ;; Citations after tool result
    (:name "citations-after-tool-result"
           :markdown "## USER:

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
Weather search results
</tool.srvtoolu_456>

## ASSISTANT:

<cite>It's currently sunny and 75°F</cite>

## CITATIONS:

### https://weather.com

Title: Weather.com
Cited text: Currently sunny with a temperature of 75 degrees Fahrenheit...
Encrypted index: xyz789"
           :dialog (((role . "user") (content . "What's the current weather?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_456")
                                                       (name . "web_search")
                                                       (input . ((query . "current weather"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_456")
                                                       (content . "Weather search results"))
                                                      ((type . "text")
                                                       (text . "It's currently sunny and 75°F")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://weather.com")
                                                                      (title . "Weather.com")
                                                                      (cited_text . "Currently sunny with a temperature of 75 degrees Fahrenheit...")
                                                                      (encrypted_index . "xyz789"))))))))))

    ;; Multiple citations test
    (:name "citations-multiple"
           :markdown "## USER:

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
Physics search results
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
Encrypted index: ghi789"
           :dialog (((role . "user") (content . "Tell me about Einstein and Newton"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_789")
                                                       (name . "web_search")
                                                       (input . ((query . "Einstein Newton physics"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_789")
                                                       (content . "Physics search results"))
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
    )

(defvar greger-tree-sitter-test-results '())
(defvar greger-tree-sitter-tests-passed 0)
(defvar greger-tree-sitter-tests-failed 0)

(defun greger-tree-sitter-test-equal (expected actual)
  "Compare two dialog structures for equality."
  (equal expected actual))

(defun greger-tree-sitter-run-single-test (test-case)
  "Run a single test case and return result."
  (let* ((name (plist-get test-case :name))
         (markdown (plist-get test-case :markdown))
         (expected (plist-get test-case :dialog))
         (start-time (current-time)))

    (message "\n=== Testing: %s ===" name)

    (condition-case err
        (let* ((actual (greger-tree-sitter-parse markdown))
               (elapsed (float-time (time-subtract (current-time) start-time))))

          ;; Check if results match
          (if (greger-tree-sitter-test-equal expected actual)
              (progn
                (message "✅ PASSED (%.3fs)" elapsed)
                (setq greger-tree-sitter-tests-passed (1+ greger-tree-sitter-tests-passed))
                (push `(:name ,name :status passed :time ,elapsed) greger-tree-sitter-test-results))
            (progn
              (message "❌ FAILED (%.3fs)" elapsed)
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp actual)
              (setq greger-tree-sitter-tests-failed (1+ greger-tree-sitter-tests-failed))
              (push `(:name ,name :status failed :time ,elapsed :expected ,expected :actual ,actual)
                    greger-tree-sitter-test-results))))

      (error
       (message "❌ ERROR: %s" (error-message-string err))
       (setq greger-tree-sitter-tests-failed (1+ greger-tree-sitter-tests-failed))
       (push `(:name ,name :status error :error ,(error-message-string err))
             greger-tree-sitter-test-results)))))

(defun greger-tree-sitter-run-all-tests ()
  "Run all test cases and report results."
  (interactive)
  (message "\n🧪 Running greger-tree-sitter comprehensive tests...")

  ;; Reset counters
  (setq greger-tree-sitter-test-results '())
  (setq greger-tree-sitter-tests-passed 0)
  (setq greger-tree-sitter-tests-failed 0)

  ;; Check if tree-sitter is available
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  ;; Run all tests
  (dolist (test-case greger-tree-sitter-test-cases)
    (greger-tree-sitter-run-single-test test-case))

  ;; Report summary
  (message "\n📊 TEST SUMMARY:")
  (message "Total tests: %d" (+ greger-tree-sitter-tests-passed greger-tree-sitter-tests-failed))
  (message "Passed: %d" greger-tree-sitter-tests-passed)
  (message "Failed: %d" greger-tree-sitter-tests-failed)

  (if (> greger-tree-sitter-tests-failed 0)
      (progn
        (message "\n❌ FAILED TESTS:")
        (dolist (result (reverse greger-tree-sitter-test-results))
          (when (eq (plist-get result :status) 'failed)
            (message "  - %s" (plist-get result :name)))))
    (message "\n🎉 ALL TESTS PASSED!"))

  ;; Return results for programmatic use
  (list :passed greger-tree-sitter-tests-passed
        :failed greger-tree-sitter-tests-failed
        :results (reverse greger-tree-sitter-test-results)))

;; Run tests automatically when loaded
(if (treesit-ready-p 'greger)
    (greger-tree-sitter-run-all-tests)
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
