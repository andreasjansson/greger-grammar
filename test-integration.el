;;; test-integration.el --- Test tree-sitter integration -*- lexical-binding: t -*-

;; Simple test to verify tree-sitter parsing works

(require 'treesit)

;; Test if we can load and use the grammar
(defun test-greger-tree-sitter ()
  "Test basic tree-sitter functionality."
  (let ((test-text "## USER:

Hello, how are you?

## ASSISTANT:

Hi there!"))

    (message "Testing tree-sitter greger parser...")

    ;; Check if parser is available
    (if (treesit-ready-p 'greger)
        (progn
          (message "Tree-sitter greger parser is ready!")

          ;; Try to parse
          (condition-case err
              (let ((tree (treesit-parse-string test-text 'greger)))
                (message "Parse successful!")
                (message "Root node: %s" (treesit-node-type (treesit-node-child tree 0)))

                ;; Show the tree structure
                (with-temp-buffer
                  (insert test-text)
                  (treesit-parser-create 'greger)
                  (message "Tree structure:")
                  (message "%s" (treesit-subtree-stat (treesit-node-child tree 0))))

                t)
            (error
             (message "Parse failed: %s" err)
             nil)))

      (message "Tree-sitter greger parser not available")
      (message "Make sure the greger.so library is in your dynamic module path")
      nil)))

;; Run the test
(test-greger-tree-sitter)
