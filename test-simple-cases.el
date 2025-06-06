;;; test-simple-cases.el --- Test basic greger-tree-sitter functionality -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun test-simple-user-message ()
  "Test simple user message parsing."
  (let ((markdown "## USER:

Hello, how are you?")
        (expected '(((role . "user") (content . "Hello, how are you?")))))
    (message "\n=== Testing simple-user-message ===")
    (let ((result (greger-tree-sitter-parse markdown)))
      ;; Reverse order since our parser puts messages in reverse
      (setq result (reverse result))
      (if (equal result expected)
          (message "✅ simple-user-message PASSED")
        (progn
          (message "❌ simple-user-message FAILED")
          (message "Expected: %S" expected)
          (message "Actual: %S" result))))))

(defun test-simple-conversation ()
  "Test simple conversation parsing."
  (let ((markdown "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?")
        (expected '(((role . "user") (content . "Hello"))
                    ((role . "assistant") (content . "Hi there! How can I help you today?")))))
    (message "\n=== Testing simple-conversation ===")
    (let ((result (greger-tree-sitter-parse markdown)))
      ;; Reverse order since our parser puts messages in reverse
      (setq result (reverse result))
      (if (equal result expected)
          (message "✅ simple-conversation PASSED")
        (progn
          (message "❌ simple-conversation FAILED")
          (message "Expected: %S" expected)
          (message "Actual: %S" result))))))

(defun test-thinking-section ()
  "Test thinking section parsing."
  (let ((markdown "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question. I can answer this directly without needing any tools.

## ASSISTANT:

2 + 2 = 4")
        (expected '(((role . "user") (content . "What's 2+2?"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "This is a simple arithmetic question. I can answer this directly without needing any tools.")) ((type . "text") (text . "2 + 2 = 4"))))))))
    (message "\n=== Testing thinking-section ===")
    (let ((result (greger-tree-sitter-parse markdown)))
      ;; Reverse order since our parser puts messages in reverse
      (setq result (reverse result))
      (if (equal result expected)
          (message "✅ thinking-section PASSED")
        (progn
          (message "❌ thinking-section FAILED")
          (message "Expected: %S" expected)
          (message "Actual: %S" result))))))

;; Run tests
(if (treesit-ready-p 'greger)
    (progn
      (test-simple-user-message)
      (test-simple-conversation)
      (test-thinking-section)
      (message "\n=== Simple tests completed ==="))
  (message "Tree-sitter greger parser not available"))
