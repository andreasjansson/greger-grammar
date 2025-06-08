;;; Simple test for implementing tool use parsing -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; Test tool use parsing
(message "Testing tool use parsing...")

(if (treesit-ready-p 'greger)
    (progn
      (message "✅ Tree-sitter greger parser is available")

      ;; Test with tool-use-single-param
      (let* ((test-content (with-temp-buffer
                             (insert-file-contents "test/corpus/tool-use-single-param.greger")
                             (buffer-string)))
             (result (greger-tree-sitter-parse test-content))
             (expected '(((role . "user")
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
                          (content . "The file contains: Hello, world!")))))

        (message "Expected:")
        (pp expected)
        (message "\nActual:")
        (pp result)

        (if (equal expected result)
            (message "✅ Tool use test PASSED")
          (message "❌ Tool use test FAILED"))))
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
