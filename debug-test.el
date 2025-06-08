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

;; Test citations-multiple parsing
(let* ((markdown (greger-read-corpus-file "citations-multiple"))
       (result (greger-tree-sitter-parse markdown)))

  (message "=== CITATIONS MULTIPLE DEBUGGING ===")
  (message "Result:")
  (pp result)

  ;; Extract just the citations from the result
  (message "\n=== CITATIONS ANALYSIS ===")
  (dolist (dialog-entry result)
    (let ((role (alist-get 'role dialog-entry))
          (content (alist-get 'content dialog-entry)))
      (when (equal role "assistant")
        (if (listp content)
            (dolist (content-item content)
              (let ((type (alist-get 'type content-item))
                    (text (alist-get 'text content-item))
                    (citations (alist-get 'citations content-item)))
                (when (equal type "text")
                  (message "Text: %s" (or text "[no text]"))
                  (when citations
                    (message "  Citations:")
                    (dolist (citation citations)
                      (let ((url (alist-get 'url citation))
                            (title (alist-get 'title citation))
                            (cited-text (alist-get 'cited_text citation)))
                        (message "    URL: %s" url)
                        (message "    Title: %s" title)
                        (message "    Cited text: %s" (substring cited-text 0 (min 50 (length cited-text))))))))))
          (message "Assistant content: %s" content))))))

(provide 'debug-test)
