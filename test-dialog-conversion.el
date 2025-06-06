;;; test-dialog-conversion.el --- Test dialog conversion from tree-sitter output -*- lexical-binding: t -*-

;; Load our tree-sitter integration
(load-file "./greger-tree-sitter.el")

;; Test cases based on the expected outputs from the original parser

(defun run-test-case (name markdown expected)
  "Run a single test case comparing tree-sitter output to expected result."
  (message "\n=== Testing: %s ===" name)
  (condition-case err
      (let ((actual (greger-tree-sitter-parse markdown)))
        (if (equal actual expected)
            (progn
              (message "✅ PASS")
              t)
          (progn
            (message "❌ FAIL")
            (message "Expected: %S" expected)
            (message "Actual:   %S" actual)
            nil)))
    (error
     (message "❌ ERROR: %s" err)
     nil)))

;; Test simple user message
(setq test-simple-user
      '((:name "simple-user-message"
         :markdown "## USER:

Hello, how are you?"
         :expected (((role . "user") (content . "Hello, how are you?"))))))

;; Test simple conversation
(setq test-simple-conversation
      '((:name "simple-conversation"
         :markdown "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?"
         :expected (((role . "user") (content . "Hello"))
                    ((role . "assistant") (content . "Hi there! How can I help you today?"))))))

;; Test tool use (simplified for now)
(setq test-tool-use
      '((:name "tool-use-simple"
         :markdown "## TOOL USE:

Name: read-file
ID: toolu_123

### path

<tool.toolu_123>
hello.txt
</tool.toolu_123>"
         :expected (((role . "assistant")
                     (content . (((type . "tool_use")
                                  (id . "toolu_123")
                                  (name . "read-file")
                                  (input . ((path . "hello.txt")))))))))))

;; Test tool result
(setq test-tool-result
      '((:name "tool-result-simple"
         :markdown "## TOOL RESULT:

ID: toolu_123

<tool.toolu_123>
Hello, world!
</tool.toolu_123>"
         :expected (((role . "user")
                     (content . (((type . "tool_result")
                                  (tool_use_id . "toolu_123")
                                  (content . "Hello, world!")))))))))

;; Test thinking section
(setq test-thinking
      '((:name "thinking-section"
         :markdown "## THINKING:

This is a simple question."
         :expected (((role . "assistant")
                     (content . (((type . "thinking")
                                  (thinking . "This is a simple question.")))))))))

;; Run all tests
(defun run-all-tests ()
  "Run all test cases."
  (let ((test-cases (append test-simple-user
                            test-simple-conversation
                            test-tool-use
                            test-tool-result
                            test-thinking))
        (passed 0)
        (total 0))

    (dolist (test test-cases)
      (setq total (1+ total))
      (when (run-test-case (plist-get test :name)
                           (plist-get test :markdown)
                           (plist-get test :expected))
        (setq passed (1+ passed))))

    (message "\n=== SUMMARY ===")
    (message "Passed: %d/%d tests" passed total)
    (if (= passed total)
        (message "🎉 All tests passed!")
      (message "💥 Some tests failed"))))

;; Check if tree-sitter is available and run tests
(if (treesit-ready-p 'greger)
    (run-all-tests)
  (message "Tree-sitter greger parser not available"))
