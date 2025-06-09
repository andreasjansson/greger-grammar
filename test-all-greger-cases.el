;;; test-all-greger-cases.el --- Comprehensive test of greger-tree-sitter against all test cases -*- lexical-binding: t -*-

(require 'ert)
(load-file "./greger-tree-sitter.el")

;; Helper function to read markdown content from corpus .txt files
(defun greger-read-corpus-file (name)
  "Read markdown content from a .txt corpus file, extracting only the input portion."
  (let ((file-path (format "./test/corpus/%s.txt" name)))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (let ((content (buffer-string)))
            ;; Find the test content between the title header and the "---" separator
            (if (string-match "=\\{10,\\}\n.*?\n=\\{10,\\}\n\n\\(\\(?:.\\|\n\\)*?\\)\n---" content)
                (match-string 1 content)
              (error "Could not parse test file format: %s" file-path))))
      (error "Corpus file not found: %s" file-path))))

;; All test cases from greger-parser-test-cases
(defconst greger-tree-sitter-test-cases
  `(
    ;; Simple user message
    (:name "simple-user-message"
           :markdown ,(greger-read-corpus-file "simple-user-message")
           :dialog (((role . "user")
                     (content . "Hello, how are you?"))))

    ;; System and user message
    (:name "system-and-user"
           :markdown ,(greger-read-corpus-file "system-and-user")
           :dialog (((role . "system")
                     (content . "You are a helpful assistant."))
                    ((role . "user")
                     (content . "What's the weather like?"))))

    ;; Simple conversation
    (:name "simple-conversation"
           :markdown ,(greger-read-corpus-file "simple-conversation")
           :dialog (((role . "user")
                     (content . "Hello"))
                    ((role . "assistant")
                     (content . "Hi there! How can I help you today?"))))

    ;; Thinking section (becomes part of assistant message)
    (:name "thinking-section"
           :markdown ,(greger-read-corpus-file "thinking-section")
           :dialog (((role . "user")
                     (content . "What's 2+2?"))
                    ((role . "assistant")
                     (content . (((type . "thinking")
                                  (thinking . "This is a simple arithmetic question. I can answer this directly without needing any tools."))
                                 ((type . "text")
                                  (text . "2 + 2 = 4")))))))

    ;; Tool use with single parameter
    (:name "tool-use-single-param"
           :markdown ,(greger-read-corpus-file "tool-use-single-param")
           :dialog (((role . "user")
                     (content . "Read the file hello.txt"))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_123")
                                  (name . "read-file")
                                  (input . ((path . "hello.txt")))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_123")
                                  (content . "Hello, world!")))))
                    ((role . "assistant")
                     (content . "The file contains: Hello, world!"))))

    ;; Tool use with multiple parameters
    (:name "tool-use-multiple-params"
           :markdown ,(greger-read-corpus-file "tool-use-multiple-params")
           :dialog (((role . "user")
                     (content . "Search for python files containing 'def main'"))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_456")
                                  (name . "ripgrep")
                                  (input . ((pattern . "def main")
                                            (file-type . "py")
                                            (context-lines . 2)))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_456")
                                  (content . "src/main.py:10:def main():\nsrc/utils.py:25:def main_helper():")))))
                    ((role . "assistant")
                     (content . "I found 2 matches for 'def main' in Python files."))))

    ;; Complex workflow with thinking, tool use, and multiple responses
    (:name "complex-workflow"
           :markdown ,(greger-read-corpus-file "complex-workflow")
           :dialog (((role . "user")
                     (content . "who's the current king of sweden?"))
                    ((role . "assistant")
                     (content . (((type . "thinking")
                                  (thinking . "The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information."))
                                 ((type . "tool_use")
                                  (id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc")
                                  (name . "search-286d2fd3")
                                  (input . ((query . "current king of Sweden 2024")
                                            (include_answer . "basic")
                                            (max_results . 3)))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc")
                                  (content . "{\"query\": \"current king of Sweden 2024\", \"answer\": \"Carl XVI Gustaf\", \"response_time\": 2.38}")))))
                    ((role . "assistant")
                     (content . "The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."))))

    ;; Multiple tool uses in sequence
    (:name "multiple-tool-uses"
           :markdown ,(greger-read-corpus-file "multiple-tool-uses")
           :dialog (((role . "user")
                     (content . "List files and read the first one"))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_111")
                                  (name . "list-directory")
                                  (input . ((path . ".")))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_111")
                                  (content . "file1.txt\nfile2.txt\nREADME.md")))))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_222")
                                  (name . "read-file")
                                  (input . ((path . "file1.txt")))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_222")
                                  (content . "This is the content of file1.")))))
                    ((role . "assistant")
                     (content . "I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\""))))


    ;; Just thinking without any other content
    (:name "thinking-only"
           :markdown ,(greger-read-corpus-file "thinking-only")
           :dialog (((role . "user")
                     (content . "Let me think about this"))
                    ((role . "assistant")
                     (content . (((type . "thinking")
                                  (thinking . "I need to consider all the options carefully before responding.")))))))

    ;; Tool use without any following content
    (:name "tool-use-only"
           :markdown ,(greger-read-corpus-file "tool-use-only")
           :dialog (((role . "user")
                     (content . "Read a file"))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_999")
                                  (name . "read-file")
                                  (input . ((path . "test.txt")))))))))

    ;; Citations basic test
    (:name "citations-basic"
           :markdown ,(greger-read-corpus-file "citations-basic")
           :dialog (((role . "user")
                     (content . "When was Claude Shannon born?"))
                    ((role . "assistant")
                     (content . (((type . "server_tool_use")
                                  (id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (name . "web_search")
                                  (input . ((query . "claude shannon birth date"))))
                                 ((type . "web_search_tool_result")
                                  (tool_use_id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (content . "[\n  {\n    \"type\": \"web_search_result\",\n    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",\n    \"title\": \"Claude Shannon - Wikipedia\",\n    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",\n    \"page_age\": \"April 30, 2025\"\n  }\n]"))
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
           :markdown ,(greger-read-corpus-file "citations-after-tool-result")
           :dialog (((role . "user")
                     (content . "What's the current weather?"))
                    ((role . "assistant")
                     (content . (((type . "server_tool_use")
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
           :markdown ,(greger-read-corpus-file "citations-multiple")
           :dialog (((role . "user")
                     (content . "Tell me about Einstein and Newton"))
                    ((role . "assistant")
                     (content . (((type . "server_tool_use")
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
                                 ((type . "text")
                                  (text . "while"))
                                 ((type . "text")
                                  (text . "Newton formulated the laws of motion")
                                  (citations . (((type . "web_search_result_location")
                                                 (url . "https://physics.com/newton")
                                                 (title . "Newton Biography")
                                                 (cited_text . "Isaac Newton formulated the three laws of motion...")
                                                 (encrypted_index . "ghi789")))))
                                 ((type . "text")
                                  (citations . (((type . "web_search_result_location")
                                                 (url . "https://physics.com/einstein")
                                                 (title . "Einstein Biography")
                                                 (cited_text . "Albert Einstein developed the theory of relativity in the early 20th century...")
                                                 (encrypted_index . "def456"))
                                                ((type . "web_search_result_location")
                                                 (url . "https://physics.com/newton")
                                                 (title . "Newton Biography")
                                                 (cited_text . "Isaac Newton formulated the three laws of motion...")
                                                 (encrypted_index . "ghi789"))))))))))

    ;; Tool result with empty lines preserved
    (:name "code-block-triple-backticks"
           :markdown ,(greger-read-corpus-file "code-block-triple-backticks")
           :dialog (((role . "user")
                     (content . "Here's some code:\n\n```\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?"))))

    ;; Mixed code blocks and real sections
    (:name "mixed-code-blocks-and-sections"
           :markdown ,(greger-read-corpus-file "mixed-code-blocks-and-sections")
           :dialog (((role . "user")
                     (content . "Here's a code example:\n\n```python\ndef example():\n    # This has ## USER: in a comment\n    print(\"## ASSISTANT: not a real header\")\n```\n\nNow please analyze it."))
                    ((role . "assistant")
                     (content . "I can see your code example."))))

    ;; Tool use with code blocks in parameters
    (:name "tool-use-with-code-in-params"
           :markdown ,(greger-read-corpus-file "tool-use-with-code-in-params")
           :dialog (((role . "user")
                     (content . "Write some Python code"))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_999")
                                  (name . "write-file")
                                  (input . ((filename . "example.py")
                                            (content . "```python\ndef main():\n    # This ## USER: comment should not break parsing\n    print(\"Hello world\")\n\nif __name__ == \"__main__\":\n    main()\n```")))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_999")
                                  (content . "File written successfully")))))
                    ((role . "assistant")
                     (content . "I've written the Python file."))))

    (:name "tool-use-with-tool-use-in-params"
           :markdown ,(greger-read-corpus-file "tool-use-with-tool-use-in-params")
           :dialog (((role . "user")
                     (content . "Write some Python code"))
                    ((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_999")
                                  (name . "write-file")
                                  (input . ((filename . "example.py")
                                            (content . "foo
<tool.toolu_123>
bar
</tool.toolu_123>")))))))
                    ((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_999")
                                  (content . "File written successfully")))))
                    ((role . "assistant")
                     (content . "I've written the Python file."))))

    ;; Nested code blocks (backticks inside code blocks)
    (:name "nested-code-blocks"
           :markdown ,(greger-read-corpus-file "nested-code-blocks")
           :dialog (((role . "user")
                     (content . "How do I use backticks in markdown?"))
                    ((role . "assistant")
                     (content . "You can use triple backticks:\n\n```\nHere's how to show `inline code` in a code block:\nUse single backticks around `your code`.\n```\n\nDoes that help?"))))

    (:name "html-comments"
           :markdown ,(greger-read-corpus-file "html-comments")
           :dialog (((role . "user")
                     (content . "Here's some code:\n\n\n\n\n```\n<!-- comment should be included -->\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?"))))

    (:name "server-tool-use-basic"
           :markdown ,(greger-read-corpus-file "server-tool-use-basic")
           :dialog (((role . "user") (content . "Search for current weather in San Francisco"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_123")
                                                       (name . "web_search")
                                                       (input . ((query . "current weather San Francisco"))))
                                                      ((type . "server_tool_result")
                                                       (tool_use_id . "srvtoolu_123")
                                                       (content . (((title . "Weather in San Francisco")
                                                                    (url . "https://weather.com/sf")
                                                                    (content . "Sunny, 72°F")))))
                                                      ((type . "text") (text . "The current weather in San Francisco is sunny and 72°F.")))))))

    (:name "server-tool-use-string-result"
           :markdown ,(greger-read-corpus-file "server-tool-use-string-result")
           :dialog (((role . "user") (content . "What's the weather like?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_456")
                                                       (name . "web_search")
                                                       (input . ((query . "weather"))))
                                                      ((type . "server_tool_result")
                                                       (tool_use_id . "srvtoolu_456")
                                                       (content . "Sunny and warm today"))
                                                      ((type . "text")
                                                       (text . "It looks like it's sunny and warm today!")))))))

    ))

;; Helper function to find test case by name
(defun greger-find-test-case (name)
  "Find a test case by name."
  (cl-find name greger-tree-sitter-test-cases
           :key (lambda (test) (plist-get test :name))
           :test #'string=))

;; Generate ERT tests for each test case
(dolist (test greger-tree-sitter-test-cases)
  (let ((test-name (plist-get test :name))
        (expected-dialog (plist-get test :dialog)))
    (eval `(ert-deftest ,(intern (format "greger-test-%s" test-name)) ()
             ,(format "Test greger-tree-sitter parsing for %s" test-name)
             (let* ((test-case (greger-find-test-case ,test-name))
                    (markdown (plist-get test-case :markdown))
                    (expected (plist-get test-case :dialog))
                    (actual (greger-tree-sitter-parse markdown)))
               (should (equal expected actual)))))))

(provide 'test-all-greger-cases)
