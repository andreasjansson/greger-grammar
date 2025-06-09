;;; Debug failing tests

(load-file "./greger-tree-sitter.el")

;; Helper function to read markdown content from corpus .txt files
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

(defun debug-test (test-name)
  "Debug a specific test case."
  (let* ((markdown (greger-read-corpus-file test-name))
         (actual (greger-tree-sitter-parse markdown)))
    (message "=== Testing %s ===" test-name)
    (message "Markdown input:")
    (message "%s" markdown)
    (message "\nActual result:")
    (pp actual)
    (message "\n")))

;; Debug the failing tests
(debug-test "server-tool-use-basic")
(debug-test "citations-basic")
(debug-test "citations-after-tool-result")
(debug-test "citations-multiple")
