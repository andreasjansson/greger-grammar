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

;; Debug function to show section types
(defun debug-show-section-types (text)
  "Show what section types the parser detects."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser))
           (sections (treesit-node-children root-node)))

      (message "Detected sections:")
      (dolist (section sections)
        (let ((section-type (treesit-node-type section)))
          (message "  %s" section-type))))))

;; Test both cases
(message "=== SECTION TYPE DETECTION ===")

(message "\n--- citations-basic ---")
(debug-show-section-types (greger-read-corpus-file "citations-basic"))

(message "\n--- tool-use-with-code-in-params ---")
(debug-show-section-types (greger-read-corpus-file "tool-use-with-code-in-params"))

(provide 'debug-test)
