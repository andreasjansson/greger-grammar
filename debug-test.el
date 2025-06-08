(load-file "./greger-tree-sitter.el")

;; Helper function to read markdown from corpus files
(defun greger-read-corpus-file (name)
  "Read markdown content from a corpus file."
  (let ((file-path (format "./test/corpus/%s.greger" name)))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (buffer-string))
      (error "Corpus file not found: %s" file-path))))

;; Override the extraction functions to add debugging
(defun debug-greger-tree-sitter--extract-tool-use (tool-use-section)
  "Debug version of tool use extraction."
  (message "    [DEBUG] Calling extract-tool-use")
  (let ((result (greger-tree-sitter--extract-tool-use tool-use-section)))
    (message "    [DEBUG] extract-tool-use returning type: %s" (alist-get 'type result))
    result))

(defun debug-greger-tree-sitter--extract-tool-result (tool-result-section)
  "Debug version of tool result extraction."
  (message "    [DEBUG] Calling extract-tool-result")
  (let ((result (greger-tree-sitter--extract-tool-result tool-result-section)))
    (message "    [DEBUG] extract-tool-result returning type: %s" (alist-get 'type result))
    result))

(defun debug-greger-tree-sitter--extract-server-tool-use (server-tool-use-section)
  "Debug version of server tool use extraction."
  (message "    [DEBUG] Calling extract-server-tool-use")
  (let ((result (greger-tree-sitter--extract-server-tool-use server-tool-use-section)))
    (message "    [DEBUG] extract-server-tool-use returning type: %s" (alist-get 'type result))
    result))

(defun debug-greger-tree-sitter--extract-server-tool-result (server-tool-result-section)
  "Debug version of server tool result extraction."
  (message "    [DEBUG] Calling extract-server-tool-result")
  (let ((result (greger-tree-sitter--extract-server-tool-result server-tool-result-section)))
    (message "    [DEBUG] extract-server-tool-result returning type: %s" (alist-get 'type result))
    result))

;; Override the main dispatch function with debugging
(defun debug-greger-tree-sitter--extract-dialog-from-node (root-node)
  "Debug version of extract dialog."
  (let* ((sections (treesit-node-children root-node))
         (dialog '())
         (pending-assistant-content '()))

    (dolist (section sections)
      (let ((section-type (treesit-node-type section)))
        (message "  Processing section: %s" section-type)
        (cond
         ((string= section-type "tool_use_section")
          (let ((tool-use-data (debug-greger-tree-sitter--extract-tool-use section)))
            (push tool-use-data pending-assistant-content)))

         ((string= section-type "tool_result_section")
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              (push `((role . "assistant") (content . ,content)) dialog))
            (setq pending-assistant-content '()))
          (let ((tool-result-data (debug-greger-tree-sitter--extract-tool-result section)))
            (push `((role . "user") (content . (,tool-result-data))) dialog)))

         ((string= section-type "server_tool_use_section")
          (let ((server-tool-use-data (debug-greger-tree-sitter--extract-server-tool-use section)))
            (push server-tool-use-data pending-assistant-content)))

         ((string= section-type "server_tool_result_section")
          (let ((server-tool-result-data (debug-greger-tree-sitter--extract-server-tool-result section)))
            (push server-tool-result-data pending-assistant-content)))

         ;; Handle other sections...
         (t (message "    Skipping section type: %s" section-type)))))

    ;; Flush any remaining pending assistant content
    (when pending-assistant-content
      (let ((content (nreverse pending-assistant-content)))
        (push `((role . "assistant") (content . ,content)) dialog)))

    (nreverse dialog)))

;; Test both cases with debugging
(message "=== EXTRACTION FUNCTION DEBUGGING ===")

(message "\n--- citations-basic ---")
(let* ((markdown (greger-read-corpus-file "citations-basic"))
       (parser (treesit-parser-create 'greger))
       (root-node))
  (with-temp-buffer
    (insert markdown)
    (setq root-node (treesit-parser-root-node parser)))
  (debug-greger-tree-sitter--extract-dialog-from-node root-node))

(message "\n--- tool-use-with-code-in-params ---")
(let* ((markdown (greger-read-corpus-file "tool-use-with-code-in-params"))
       (parser (treesit-parser-create 'greger))
       (root-node))
  (with-temp-buffer
    (insert markdown)
    (setq root-node (treesit-parser-root-node parser)))
  (debug-greger-tree-sitter--extract-dialog-from-node root-node))

(provide 'debug-test)
