;;; debug-test.el --- Debug specific test case

(require 'ert)
(require 'cl-lib)
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

;; Debug simple user message
(let* ((markdown (greger-read-corpus-file "simple-user-message"))
       (expected '(((role . "user")
                    (content . "Hello, how are you?"))))
       (actual (greger-tree-sitter-parse markdown)))
  (message "Markdown input: %S" markdown)
  (message "Expected: %S" expected)
  (message "Actual: %S" actual)
  (message "Equal? %S" (equal expected actual)))

;; Also debug the tree structure
(unless (treesit-ready-p 'greger)
  (error "Tree-sitter greger parser not available"))

(with-temp-buffer
  (insert "## USER:\n\nHello, how are you?\n")
  (let* ((parser (treesit-parser-create 'greger))
         (root-node (treesit-parser-root-node parser)))
    (message "=== Tree structure ===")
    (debug-print-node root-node 0)))

(defun debug-print-node (node level)
  "Print node structure recursively."
  (let ((indent (make-string (* level 2) ?\s))
        (node-type (treesit-node-type node))
        (node-text (treesit-node-text node)))
    (message "%s%s: %S" indent node-type
             (if (< (length node-text) 50)
                 node-text
               (concat (substring node-text 0 47) "...")))
    (dolist (child (treesit-node-children node))
      (debug-print-node child (1+ level)))))
