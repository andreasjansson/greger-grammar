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

;; Focus specifically on the third citations section
(let* ((markdown (greger-read-corpus-file "citations-multiple"))
       (parser (treesit-parser-create 'greger))
       (root-node))

  (with-temp-buffer
    (insert markdown)
    (setq root-node (treesit-parser-root-node parser)))

  (message "=== CITATIONS MULTIPLE - THIRD SECTION DEBUG ===")

  ;; Find the third citations section
  (let ((sections (treesit-node-children root-node)))
    (dolist (section sections)
      (when (string= (treesit-node-type section) "citations_section")
        (let ((section-start (treesit-node-start section))
              (section-end (treesit-node-end section)))
          (message "Citations section: [%d, %d]" section-start section-end)

          ;; If this is the third one (starting around position 49)
          (when (> section-start 45)
            (message "=== Processing third citations section ===")
            (let ((children (treesit-node-children section)))
              (message "Children count: %d" (length children))
              (dotimes (i (length children))
                (let* ((child (nth i children))
                       (node-type (treesit-node-type child))
                       (node-text (treesit-node-text child))
                       (node-start (treesit-node-start child))
                       (node-end (treesit-node-end child)))
                  (message "  Child %d: %s [%d, %d]" i node-type node-start node-end)
                  (message "    Text: %s" (replace-regexp-in-string "\n" "\\\\n" node-text)))))

            ;; Now test the extraction
            (message "=== Extracting citations ===")
            (let ((extracted (greger-tree-sitter--extract-citations-section section)))
              (message "Extracted result:")
              (pp extracted))))))))

(provide 'debug-test)
