;;; Debug tree-sitter nodes

(require 'treesit)

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

(let* ((markdown (greger-read-corpus-file "server-tool-use-basic")))
  (with-temp-buffer
    (insert markdown)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (message "=== Tree-sitter nodes ===")
      (dolist (child (treesit-node-children root-node))
        (message "Node type: %s" (treesit-node-type child))))))
