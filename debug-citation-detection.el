;;; Debug citation detection

(load-file "./greger-tree-sitter.el")

(defun greger-read-corpus-file (name)
  "Read markdown content from a .txt corpus file, extracting only the input portion."
  (let ((file-path (format "./test/corpus/%s.txt" name)))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (let ((content (buffer-string)))
            ;; Find the test content between the title header and the "---" separator
            (if (string-match "=\\{10,\\}\n.*?\n=\\{10,\\}\n\n\\(\\(?:.\\|\n\\)*?\\)\n---" content)
                (match-string 1 content)
              (error "Could not parse test file format: %s" file-path))))
      (error "Corpus file not found: %s" file-path))))

;; Test the citation detection on raw entries
(let* ((markdown (greger-read-corpus-file "citations-after-tool-result"))
       (parser (treesit-parser-create 'greger))
       (root-node (progn (with-temp-buffer
                           (insert markdown)
                           (treesit-parser-root-node parser))))
       (raw-entries '()))
  ;; Extract raw entries
  (dolist (child (treesit-node-children root-node))
    (let ((entry (greger-tree-sitter--extract-entry-from-node child)))
      (when entry
        (push entry raw-entries))))
  (setq raw-entries (nreverse raw-entries))

  (message "Raw entries:")
  (dolist (entry raw-entries)
    (pp entry))
  (message "Has citations: %s" (greger-tree-sitter--has-citations-p raw-entries)))
