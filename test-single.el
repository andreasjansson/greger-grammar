;;; test-single.el --- Test single case -*- lexical-binding: t -*-

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

(let* ((markdown (greger-read-corpus-file "tool-use-single-param"))
       (expected '(((role . "user")
                    (content . "Read the file hello.txt"))
                   ((role . "assistant")
                    (content . (((type . "tool_use")
                                 (id . "toolu_123")
                                 (name . "read-file")
                                 (input . ((path . "hello.txt")))))))
                   ((role . "user")
                    (content . (((type . "tool_result")
                                 (tool_use_id . "toolu_123")
                                 (content . "Hello, world!")))))
                   ((role . "assistant")
                    (content . "The file contains: Hello, world!"))))
       (actual (greger-tree-sitter-parse markdown)))
  (message "Expected length: %d" (length expected))
  (message "Actual length: %d" (length actual))
  (dotimes (i (length expected))
    (let ((exp-item (nth i expected))
          (act-item (nth i actual)))
      (message "Item %d equal: %s" i (equal exp-item act-item))
      (unless (equal exp-item act-item)
        (message "Expected item %d: %s" i exp-item)
        (message "Actual item %d: %s" i act-item)))))
