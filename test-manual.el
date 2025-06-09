;;; test-manual.el --- Manual test runner -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")
(require 'ert)

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

(defun test-case (name expected)
  "Test a single case and return whether it passed."
  (let* ((markdown (greger-read-corpus-file name))
         (actual (greger-tree-sitter-parse markdown)))
    (if (equal expected actual)
        (progn (message "PASS: %s" name) t)
      (progn (message "FAIL: %s" name) nil))))

(let ((passed 0)
      (total 0))
  ;; Simple cases
  (when (test-case "simple-user-message"
                   '(((role . "user") (content . "Hello, how are you?"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "system-and-user"
                   '(((role . "system") (content . "You are a helpful assistant."))
                     ((role . "user") (content . "What's the weather like?"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "simple-conversation"
                   '(((role . "user") (content . "Hello"))
                     ((role . "assistant") (content . "Hi there! How can I help you today?"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "thinking-only"
                   '(((role . "user") (content . "Let me think about this"))
                     ((role . "assistant") (content . (((type . "thinking")
                                                        (thinking . "I need to consider all the options carefully before responding.")))))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "thinking-section"
                   '(((role . "user") (content . "What's 2+2?"))
                     ((role . "assistant") (content . (((type . "thinking")
                                                        (thinking . "This is a simple arithmetic question. I can answer this directly without needing any tools."))
                                                       ((type . "text")
                                                        (text . "2 + 2 = 4")))))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "tool-use-only"
                   '(((role . "user") (content . "Read a file"))
                     ((role . "assistant") (content . (((type . "tool_use")
                                                        (id . "toolu_999")
                                                        (name . "read-file")
                                                        (input . ((path . "test.txt")))))))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "tool-use-single-param"
                   '(((role . "user") (content . "Read the file hello.txt"))
                     ((role . "assistant") (content . (((type . "tool_use")
                                                        (id . "toolu_123")
                                                        (name . "read-file")
                                                        (input . ((path . "hello.txt")))))))
                     ((role . "user") (content . (((type . "tool_result")
                                                   (tool_use_id . "toolu_123")
                                                   (content . "Hello, world!")))))
                     ((role . "assistant") (content . "The file contains: Hello, world!"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "tool-use-multiple-params"
                   '(((role . "user") (content . "Search for python files containing 'def main'"))
                     ((role . "assistant") (content . (((type . "tool_use")
                                                        (id . "toolu_456")
                                                        (name . "ripgrep")
                                                        (input . ((pattern . "def main")
                                                                  (file-type . "py")
                                                                  (context-lines . 2)))))))
                     ((role . "user") (content . (((type . "tool_result")
                                                   (tool_use_id . "toolu_456")
                                                   (content . "src/main.py:10:def main():\nsrc/utils.py:25:def main_helper():")))))
                     ((role . "assistant") (content . "I found 2 matches for 'def main' in Python files."))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "code-block-triple-backticks"
                   '(((role . "user") (content . "Here's some code:\n\n```\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "mixed-code-blocks-and-sections"
                   '(((role . "user") (content . "Here's a code example:\n\n```python\ndef example():\n    # This has ## USER: in a comment\n    print(\"## ASSISTANT: not a real header\")\n```\n\nNow please analyze it."))
                     ((role . "assistant") (content . "I can see your code example."))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "nested-code-blocks"
                   '(((role . "user") (content . "How do I use backticks in markdown?"))
                     ((role . "assistant") (content . "You can use triple backticks:\n\n```\nHere's how to show `inline code` in a code block:\nUse single backticks around `your code`.\n```\n\nDoes that help?"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "html-comments"
                   '(((role . "user") (content . "Here's some code:\n\n\n\n\n```\n<!-- comment should be included -->\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?"))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "tool-use-with-code-in-params"
                   '(((role . "user") (content . "Write some Python code"))
                     ((role . "assistant") (content . (((type . "tool_use")
                                                        (id . "toolu_999")
                                                        (name . "write-file")
                                                        (input . ((filename . "example.py")
                                                                  (content . "```python\ndef main():\n    # This ## USER: comment should not break parsing\n    print(\"Hello world\")\n\nif __name__ == \"__main__\":\n    main()\n```")))))))
                     ((role . "user") (content . (((type . "tool_result")
                                                   (tool_use_id . "toolu_999")
                                                   (content . "File written successfully")))))
                     ((role . "assistant") (content . "I've written the Python file."))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "tool-use-with-tool-use-in-params"
                   '(((role . "user") (content . "Write some Python code"))
                     ((role . "assistant") (content . (((type . "tool_use")
                                                        (id . "toolu_999")
                                                        (name . "write-file")
                                                        (input . ((filename . "example.py")
                                                                  (content . "foo\n<tool.toolu_123>\nbar\n</tool.toolu_123>")))))))
                     ((role . "user") (content . (((type . "tool_result")
                                                   (tool_use_id . "toolu_999")
                                                   (content . "File written successfully")))))
                     ((role . "assistant") (content . "I've written the Python file."))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (when (test-case "server-tool-use-string-result"
                   '(((role . "user") (content . "What's the weather like?"))
                     ((role . "assistant") (content . (((type . "server_tool_use")
                                                        (id . "srvtoolu_456")
                                                        (name . "web_search")
                                                        (input . ((query . "weather"))))
                                                       ((type . "server_tool_result")
                                                        (tool_use_id . "srvtoolu_456")
                                                        (content . "Sunny and warm today"))
                                                       ((type . "text")
                                                        (text . "It looks like it's sunny and warm today!")))))))
    (setq passed (1+ passed)))
  (setq total (1+ total))

  (message "\nPassed: %d/%d tests" passed total))
