;;; Debug server tool basic

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

(let* ((markdown (greger-read-corpus-file "server-tool-use-basic"))
       (actual (greger-tree-sitter-parse markdown)))
  (message "=== Server Tool Use Basic Test ===")
  (message "Actual result:")
  (pp actual))
