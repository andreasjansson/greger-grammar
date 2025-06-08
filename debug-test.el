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

;; Test just one case in isolation
(defconst debug-test-cases
  `((:name "tool-use-with-code-in-params"
           :markdown ,(greger-read-corpus-file "tool-use-with-code-in-params"))))

(message "=== ISOLATED TEST DEBUG ===")

(dolist (test-case debug-test-cases)
  (let* ((name (plist-get test-case :name))
         (markdown (plist-get test-case :markdown))
         (result (greger-tree-sitter-parse markdown)))

    (message "\n--- Testing: %s ---" name)

    ;; Look at tool use and tool result types specifically
    (dolist (dialog-entry result)
      (let ((role (alist-get 'role dialog-entry))
            (content (alist-get 'content dialog-entry)))
        (when (equal role "assistant")
          (if (listp content)
              (dolist (content-item content)
                (let ((type (alist-get 'type content-item)))
                  (when (or (string= type "tool_use")
                            (string= type "server_tool_use"))
                    (message "  Found tool use type: %s" type))))
            ;; Single content item
            (message "  Assistant content: %s" content)))
        (when (equal role "user")
          (if (listp content)
              (dolist (content-item content)
                (let ((type (alist-get 'type content-item)))
                  (when (or (string= type "tool_result")
                            (string= type "server_tool_result")
                            (string= type "web_search_tool_result"))
                    (message "  Found tool result type: %s" type))))
            ;; Single content item
            (message "  User content: %s" content)))))))

(provide 'debug-test)
