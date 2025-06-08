(load-file "./greger-tree-sitter.el")

;; Test just the tool-use-with-code-in-params case
(defconst greger-test-case
  `(:name "tool-use-with-code-in-params"
          :markdown ,(with-temp-buffer
                       (insert-file-contents "./test/corpus/tool-use-with-code-in-params.greger")
                       (buffer-string))
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
                    (content . "I've written the Python file.")))))

(let* ((markdown (plist-get greger-test-case :markdown))
       (expected (plist-get greger-test-case :dialog))
       (actual (greger-tree-sitter-parse markdown)))

  (message "=== ISOLATED TEST: tool-use-with-code-in-params ===")
  (message "Expected:")
  (pp expected)
  (message "\nActual:")
  (pp actual)
  (message "\nEqual? %s" (equal expected actual)))

(provide 'debug-test)
