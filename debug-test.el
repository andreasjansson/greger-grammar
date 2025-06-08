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

;; Test citations-multiple parsing and focus on the problematic part
(let* ((markdown (greger-read-corpus-file "citations-multiple"))
       (result (greger-tree-sitter-parse markdown)))

  (message "=== CITATIONS MULTIPLE DEBUG ===")

  ;; Look at the assistant content items
  (let ((assistant-content (alist-get 'content (nth 1 result))))
    (message "Assistant content has %d items" (length assistant-content))

    ;; Look at each item
    (dotimes (i (length assistant-content))
      (let* ((item (nth i assistant-content))
             (type (alist-get 'type item))
             (text (alist-get 'text item))
             (citations (alist-get 'citations item)))
        (message "\nItem %d:" i)
        (message "  Type: %s" type)
        (message "  Text: %s" (if text (substring text 0 (min 50 (length text))) "[no text]"))
        (when citations
          (message "  Citations count: %d" (length citations))
          (dotimes (j (length citations))
            (let* ((citation (nth j citations))
                   (url (alist-get 'url citation))
                   (title (alist-get 'title citation)))
              (message "    Citation %d: URL=%s, Title=%s" j url title))))))))

(provide 'debug-test)
