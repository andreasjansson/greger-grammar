;;; test-complex-cases.el --- Test complex cases for greger tree-sitter integration -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; Test tool use with multiple parameters
(defun test-tool-multiple-params ()
  "Test tool use with multiple parameters."
  (let ((markdown "## TOOL USE:

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
</tool.toolu_456>"))
    (let ((result (greger-tree-sitter-parse markdown)))
      (message "Multiple param result: %S" result)
      result)))

;; Test full conversation with tool use
(defun test-full-conversation ()
  "Test a full conversation with tool use."
  (let ((markdown "## USER:

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

The file contains: Hello, world!"))
    (let ((result (greger-tree-sitter-parse markdown)))
      (message "Full conversation result length: %d" (length result))
      (dolist (msg result)
        (message "  %s: %S" (alist-get 'role msg) (alist-get 'content msg)))
      result)))

;; Test thinking + tool use combination
(defun test-thinking-with-tools ()
  "Test thinking section followed by tool use."
  (let ((markdown "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question.

## TOOL USE:

Name: calculate
ID: toolu_calc

### expression

<tool.toolu_calc>
2+2
</tool.toolu_calc>"))
    (let ((result (greger-tree-sitter-parse markdown)))
      (message "Thinking + tools result: %S" result)
      result)))

;; Run all complex tests
(defun run-complex-tests ()
  "Run all complex test cases."
  (message "\n=== COMPLEX TESTS ===")
  (condition-case err
      (progn
        (test-tool-multiple-params)
        (test-full-conversation)
        (test-thinking-with-tools)
        (message "✅ All complex tests completed successfully"))
    (error
     (message "❌ Complex test error: %s" err))))

;; Check if tree-sitter is available and run tests
(if (treesit-ready-p 'greger)
    (run-complex-tests)
  (message "Tree-sitter greger parser not available"))
