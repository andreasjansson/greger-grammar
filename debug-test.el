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

;; Test both cases to see which extraction functions are called
(message "=== FUNCTION CALL DEBUGGING ===")

(message "\n--- citations-basic ---")
(let ((result (greger-tree-sitter-parse (greger-read-corpus-file "citations-basic"))))
  (message "Citations basic result length: %d" (length result)))

(message "\n--- tool-use-with-code-in-params ---")
(let ((result (greger-tree-sitter-parse (greger-read-corpus-file "tool-use-with-code-in-params"))))
  (message "Tool use result length: %d" (length result))
  ;; Check the types
  (dolist (dialog-entry result)
    (let ((role (alist-get 'role dialog-entry))
          (content (alist-get 'content dialog-entry)))
      (when (equal role "assistant")
        (if (listp content)
            (dolist (content-item content)
              (let ((type (alist-get 'type content-item)))
                (when type
                  (message "  Assistant content type: %s" type))))
          (message "  Assistant content: %s" content)))
      (when (equal role "user")
        (if (listp content)
            (dolist (content-item content)
              (let ((type (alist-get 'type content-item)))
                (when type
                  (message "  User content type: %s" type))))
          (message "  User content: %s" content))))))

(provide 'debug-test)
