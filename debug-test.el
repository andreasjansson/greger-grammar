(load-file "./greger-tree-sitter.el")

;; Debug the parsing process by adding instrumentation
(defun debug-greger-tree-sitter--extract-dialog-from-node (root-node)
  "Extract dialog structure from parsed greger conversation with debugging."
  (let* ((sections (treesit-node-children root-node))
         (dialog '())
         (pending-assistant-content '()))

    (dolist (section sections)
      (let ((section-type (treesit-node-type section)))
        (message "Processing section type: %s" section-type)
        (cond
         ((string= section-type "user_section")
          ;; Flush any pending assistant content before processing user section
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              ;; Simplify content if it's just a single text block
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

         ((string= section-type "tool_use_section")
          ;; Add tool use to pending assistant content
          (message "Extracting tool use section...")
          (let ((tool-use-data (greger-tree-sitter--extract-tool-use section)))
            (message "Tool use data type: %s" (alist-get 'type tool-use-data))
            (push tool-use-data pending-assistant-content)))

         ((string= section-type "tool_result_section")
          ;; Flush any pending assistant content and add tool result as user content
          (message "Extracting tool result section...")
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              ;; Simplify content if it's just a single text block
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
            (message "Tool result data type: %s" (alist-get 'type tool-result-data))
            (push `((role . "user")
                    (content . (,tool-result-data)))
                  dialog)))

         ((string= section-type "assistant_section")
          ;; Add assistant text to pending content (don't flush yet)
          (let ((assistant-text (greger-tree-sitter--extract-section-text section)))
            (when (> (length (string-trim assistant-text)) 0)
              (push `((type . "text")
                      (text . ,assistant-text))
                    pending-assistant-content)))))))

    ;; Flush any remaining pending assistant content
    (when pending-assistant-content
      (let ((content (nreverse pending-assistant-content)))
        ;; Simplify content if it's just a single text block
        (if (and (= (length content) 1)
                 (equal (alist-get 'type (car content)) "text"))
            (push `((role . "assistant")
                    (content . ,(alist-get 'text (car content))))
                  dialog)
          (push `((role . "assistant")
                  (content . ,content))
                dialog))))

    (nreverse dialog)))

;; Test the tool-use-with-code-in-params case specifically with debug
(let* ((markdown (with-temp-buffer
                   (insert-file-contents "./test/corpus/tool-use-with-code-in-params.greger")
                   (buffer-string)))
       (parser (treesit-parser-create 'greger))
       (root-node))
  (with-temp-buffer
    (insert markdown)
    (setq root-node (treesit-parser-root-node parser)))

  (message "=== DEBUGGING TOOL USE WITH CODE IN PARAMS ===")
  (let ((result (debug-greger-tree-sitter--extract-dialog-from-node root-node)))
    (message "Final result:")
    (pp result)))

(provide 'debug-test)
