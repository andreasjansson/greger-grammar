(load-file "./greger-tree-sitter.el")

;; Override the main processing function to add debug output
(defun greger-tree-sitter--extract-dialog-from-node-debug (root-node)
  "Extract dialog structure from parsed greger conversation with debug output."
  (let* ((sections (treesit-node-children root-node))
         (dialog '())
         (pending-assistant-content '()))

    (dolist (section sections)
      (let ((section-type (treesit-node-type section)))
        (message "Processing section type: %s" section-type)
        (cond
         ((string= section-type "user_section")
          (message "  -> USER section")
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              (if (and (= (length content) 1)
                       (equal (alist-get 'type (car content)) "text"))
                  (push `((role . "assistant")
                          (content . ,(alist-get 'text (car content))))
                        dialog)
                (push `((role . "assistant")
                        (content . ,content))
                      dialog)))
            (setq pending-assistant-content '()))
          (push (greger-tree-sitter--extract-user-section section) dialog))

         ((string= section-type "assistant_section")
          (message "  -> ASSISTANT section")
          (let ((assistant-text (greger-tree-sitter--extract-section-text section)))
            (when (> (length (string-trim assistant-text)) 0)
              (push `((type . "text")
                      (text . ,assistant-text))
                    pending-assistant-content))))

         ((string= section-type "system_section")
          (message "  -> SYSTEM section")
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              (if (and (= (length content) 1)
                       (equal (alist-get 'type (car content)) "text"))
                  (push `((role . "assistant")
                          (content . ,(alist-get 'text (car content))))
                        dialog)
                (push `((role . "assistant")
                        (content . ,content))
                      dialog)))
            (setq pending-assistant-content '()))
          (push (greger-tree-sitter--extract-system-section section) dialog))

         ((string= section-type "thinking_section")
          (message "  -> THINKING section")
          (let ((thinking-content (greger-tree-sitter--extract-section-text section)))
            (push `((type . "thinking")
                    (thinking . ,thinking-content))
                  pending-assistant-content)))

         ((string= section-type "tool_use_section")
          (message "  -> TOOL USE section")
          (let ((tool-use-data (greger-tree-sitter--extract-tool-use section)))
            (message "    Tool use data type: %s" (alist-get 'type tool-use-data))
            (push tool-use-data pending-assistant-content)))

         ((string= section-type "tool_result_section")
          (message "  -> TOOL RESULT section")
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              (if (and (= (length content) 1)
                       (equal (alist-get 'type (car content)) "text"))
                  (push `((role . "assistant")
                          (content . ,(alist-get 'text (car content))))
                        dialog)
                (push `((role . "assistant")
                        (content . ,content))
                      dialog)))
            (setq pending-assistant-content '()))
          (let ((tool-result-data (greger-tree-sitter--extract-tool-result section)))
            (message "    Tool result data type: %s" (alist-get 'type tool-result-data))
            (push `((role . "user")
                    (content . (,tool-result-data)))
                  dialog)))

         ((string= section-type "server_tool_use_section")
          (message "  -> SERVER TOOL USE section")
          (let ((server-tool-use-data (greger-tree-sitter--extract-server-tool-use section)))
            (message "    Server tool use data type: %s" (alist-get 'type server-tool-use-data))
            (push server-tool-use-data pending-assistant-content)))

         ((string= section-type "server_tool_result_section")
          (message "  -> SERVER TOOL RESULT section")
          (let ((server-tool-result-data (greger-tree-sitter--extract-server-tool-result section)))
            (message "    Server tool result data type: %s" (alist-get 'type server-tool-result-data))
            (push server-tool-result-data pending-assistant-content)))

         ((string= section-type "citations_section")
          (message "  -> CITATIONS section")
          (let ((citations-data (greger-tree-sitter--extract-citations-section section)))
            (dolist (item citations-data)
              (push item pending-assistant-content)))))))

    ;; Flush any remaining pending assistant content
    (when pending-assistant-content
      (let ((content (nreverse pending-assistant-content)))
        (if (and (= (length content) 1)
                 (equal (alist-get 'type (car content)) "text"))
            (push `((role . "assistant")
                    (content . ,(alist-get 'text (car content))))
                  dialog)
          (push `((role . "assistant")
                  (content . ,content))
                dialog))))

    (nreverse dialog)))

(defun debug-tool-use-sections ()
  "Debug tool use section types."
  (let* ((text (with-temp-buffer
                 (insert-file-contents "./test/corpus/tool-use-with-code-in-params.greger")
                 (buffer-string))))
    (with-temp-buffer
      (insert text)
      (let* ((parser (treesit-parser-create 'greger))
             (root-node (treesit-parser-root-node parser)))
        (message "=== DEBUG: tool-use-with-code-in-params.greger ===")
        (greger-tree-sitter--extract-dialog-from-node-debug root-node)))))

(debug-tool-use-sections)
