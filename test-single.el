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

(let* ((markdown (greger-read-corpus-file "complex-workflow"))
       (expected '(((role . "user")
                    (content . "who's the current king of sweden?"))
                   ((role . "assistant")
                    (content . (((type . "thinking")
                                 (thinking . "The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information."))
                                ((type . "tool_use")
                                 (id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc")
                                 (name . "search-286d2fd3")
                                 (input . ((query . "current king of Sweden 2024")
                                           (include_answer . "basic")
                                           (max_results . 3)))))))
                   ((role . "user")
                    (content . (((type . "tool_result")
                                 (tool_use_id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc")
                                 (content . "{\"query\": \"current king of Sweden 2024\", \"answer\": \"Carl XVI Gustaf\", \"response_time\": 2.38}")))))
                   ((role . "assistant")
                    (content . "The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."))))
       (actual (greger-tree-sitter-parse markdown)))
  (message "Expected: %s" expected)
  (message "Actual: %s" actual)
  (message "Equal: %s" (equal expected actual)))
