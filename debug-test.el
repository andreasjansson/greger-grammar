;;; debug-test.el --- Debug simple parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun debug-simple-user ()
  "Debug simple user message parsing."
  (let ((markdown "## USER:

Hello, how are you?"))
    (message "\n=== Debugging simple user message ===")
    (message "Input: %S" markdown)
    (let ((result (greger-tree-sitter-parse markdown)))
      (message "Raw result: %S" result))))

(defun debug-simple-conversation ()
  "Debug simple conversation parsing."
  (let ((markdown "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?"))
    (message "\n=== Debugging simple conversation ===")
    (message "Input: %S" markdown)
    (let ((result (greger-tree-sitter-parse markdown)))
      (message "Raw result: %S" result)
      (message "Number of messages: %d" (length result)))))

;; Run debug
(if (treesit-ready-p 'greger)
    (progn
      (debug-simple-user)
      (debug-simple-conversation))
  (message "Tree-sitter greger parser not available"))
