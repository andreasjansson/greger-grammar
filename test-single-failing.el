;;; Test single failing case

(require 'ert)
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

(ert-deftest greger-test-server-tool-use-basic ()
  "Test greger-tree-sitter parsing for server-tool-use-basic"
  (let* ((markdown (greger-read-corpus-file "server-tool-use-basic"))
         (expected '(((role . "user") (content . "Search for current weather in San Francisco"))
                     ((role . "assistant") (content . (((type . "server_tool_use")
                                                        (id . "srvtoolu_123")
                                                        (name . "web_search")
                                                        (input . ((query . "current weather San Francisco"))))
                                                       ((type . "server_tool_result")
                                                        (tool_use_id . "srvtoolu_123")
                                                        (content . (((title . "Weather in San Francisco")
                                                                     (url . "https://weather.com/sf")
                                                                     (content . "Sunny, 72°F")))))
                                                       ((type . "text") (text . "The current weather in San Francisco is sunny and 72°F.")))))))
         (actual (greger-tree-sitter-parse markdown)))
    (should (equal expected actual))))

(ert-run-tests-batch-and-exit)
