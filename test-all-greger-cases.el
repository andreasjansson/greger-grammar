;;; Test for server tool use and citations -*- lexical-binding: t -*-

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

(message "Testing server tool use and citations...")

(if (treesit-ready-p 'greger)
    (progn
      (message "✅ Tree-sitter greger parser is available")

      ;; Test citations-basic
      (let* ((test-content (greger-read-corpus-file "citations-basic"))
             (result (greger-tree-sitter-parse test-content)))
        (message "Citations-basic parse result:")
        (pp result)))
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
