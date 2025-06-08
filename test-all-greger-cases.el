;;; test-all-greger-cases.el --- Comprehensive test of greger-tree-sitter against all test cases -*- lexical-binding: t -*-

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

;; All test cases from greger-parser-test-cases
(defconst greger-tree-sitter-test-cases
  (list
    ;; Simple user message
    (list :name "simple-user-message"
           :markdown (greger-read-corpus-file "simple-user-message")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "Hello, how are you?"))))

    ;; System and user message
    (list :name "system-and-user"
           :markdown (greger-read-corpus-file "system-and-user")
           :dialog (list (list (cons 'role "system")
                     (cons 'content "You are a helpful assistant."))
                    (list (cons 'role "user")
                     (cons 'content "What's the weather like?"))))

    ;; Simple conversation
    (list :name "simple-conversation"
           :markdown (greger-read-corpus-file "simple-conversation")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "Hello"))
                    (list (cons 'role "assistant")
                     (cons 'content "Hi there! How can I help you today?"))))

    ;; Thinking section (becomes part of assistant message)
    (list :name "thinking-section"
           :markdown (greger-read-corpus-file "thinking-section")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "What's 2+2?"))
                    (list (cons 'role "assistant")
                     (cons 'content (list (list (cons 'type "thinking")
                                  (cons 'thinking "This is a simple arithmetic question. I can answer this directly without needing any tools."))
                                 (list (cons 'type "text")
                                  (cons 'text "2 + 2 = 4")))))))

    ;; Tool use with single parameter
    (list :name "tool-use-single-param"
           :markdown (greger-read-corpus-file "tool-use-single-param")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "Read the file hello.txt"))
                    (list (cons 'role "assistant")
                     (cons 'content (list (list (cons 'type "tool_use")
                                  (cons 'id "toolu_123")
                                  (cons 'name "read-file")
                                  (cons 'input (list (cons 'path "hello.txt")))))))
                    (list (cons 'role "user")
                     (cons 'content (list (list (cons 'type "tool_result")
                                  (cons 'tool_use_id "toolu_123")
                                  (cons 'content "Hello, world!")))))
                    (list (cons 'role "assistant")
                     (cons 'content "The file contains: Hello, world!"))))

    ;; Just thinking without any other content
    (list :name "thinking-only"
           :markdown (greger-read-corpus-file "thinking-only")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "Let me think about this"))
                    (list (cons 'role "assistant")
                     (cons 'content (list (list (cons 'type "thinking")
                                  (cons 'thinking "I need to consider all the options carefully before responding.")))))))

    ;; Tool use without any following content
    (list :name "tool-use-only"
           :markdown (greger-read-corpus-file "tool-use-only")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "Read a file"))
                    (list (cons 'role "assistant")
                     (cons 'content (list (list (cons 'type "tool_use")
                                  (cons 'id "toolu_999")
                                  (cons 'name "read-file")
                                  (cons 'input (list (cons 'path "test.txt")))))))))

    ;; Citations basic test
    (list :name "citations-basic"
           :markdown (greger-read-corpus-file "citations-basic")
           :dialog (list (list (cons 'role "user")
                     (cons 'content "When was Claude Shannon born?"))
                    (list (cons 'role "assistant")
                     (cons 'content (list (list (cons 'type "server_tool_use")
                                  (cons 'id "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (cons 'name "web_search")
                                  (cons 'input (list (cons 'query "claude shannon birth date"))))
                                 (list (cons 'type "web_search_tool_result")
                                  (cons 'tool_use_id "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                  (cons 'content "Search results about Claude Shannon"))
                                 (list (cons 'type "text")
                                  (cons 'text "Based on the search results,"))
                                 (list (cons 'type "text")
                                  (cons 'text "Claude Shannon was born on April 30, 1916, in Petoskey, Michigan")
                                  (cons 'citations (list (list (cons 'type "web_search_result_location")
                                                 (cons 'url "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                 (cons 'title "Claude Shannon - Wikipedia")
                                                 (cons 'cited_text "Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...")
                                                 (cons 'encrypted_index "Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")))))))))

    ))

(defvar greger-tree-sitter-test-results '())
(defvar greger-tree-sitter-tests-passed 0)
(defvar greger-tree-sitter-tests-failed 0)

(defun greger-tree-sitter-test-equal (expected actual)
  "Compare two dialog structures for equality."
  (equal expected actual))

(defun greger-tree-sitter-run-single-test (test-case)
  "Run a single test case and return result."
  (let* ((name (plist-get test-case :name))
         (markdown (plist-get test-case :markdown))
         (expected (plist-get test-case :dialog))
         (start-time (current-time)))

    (message "\n=== Testing: %s ===" name)

    (condition-case err
        (let* ((actual (greger-tree-sitter-parse markdown))
               (elapsed (float-time (time-subtract (current-time) start-time))))

          ;; Check if results match
          (if (greger-tree-sitter-test-equal expected actual)
              (progn
                (message "✅ PASSED (%.3fs)" elapsed)
                (setq greger-tree-sitter-tests-passed (1+ greger-tree-sitter-tests-passed))
                (push `(:name ,name :status passed :time ,elapsed) greger-tree-sitter-test-results))
            (progn
              (message "❌ FAILED (%.3fs)" elapsed)
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp actual)
              (setq greger-tree-sitter-tests-failed (1+ greger-tree-sitter-tests-failed))
              (push `(:name ,name :status failed :time ,elapsed :expected ,expected :actual ,actual)
                    greger-tree-sitter-test-results))))

      (error
       (message "❌ ERROR: %s" (error-message-string err))
       (setq greger-tree-sitter-tests-failed (1+ greger-tree-sitter-tests-failed))
       (push `(:name ,name :status error :error ,(error-message-string err))
             greger-tree-sitter-test-results)))))

(defun greger-tree-sitter-run-all-tests ()
  "Run all test cases and report results."
  (interactive)
  (message "\n🧪 Running greger-tree-sitter comprehensive tests...")

  ;; Reset counters
  (setq greger-tree-sitter-test-results '())
  (setq greger-tree-sitter-tests-passed 0)
  (setq greger-tree-sitter-tests-failed 0)

  ;; Check if tree-sitter is available
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  ;; Run all tests
  (dolist (test-case greger-tree-sitter-test-cases)
    (greger-tree-sitter-run-single-test test-case))

  ;; Report summary
  (message "\n📊 TEST SUMMARY:")
  (message "Total tests: %d" (+ greger-tree-sitter-tests-passed greger-tree-sitter-tests-failed))
  (message "Passed: %d" greger-tree-sitter-tests-passed)
  (message "Failed: %d" greger-tree-sitter-tests-failed)

  (if (> greger-tree-sitter-tests-failed 0)
      (progn
        (message "\n❌ FAILED TESTS:")
        (dolist (result (reverse greger-tree-sitter-test-results))
          (when (eq (plist-get result :status) 'failed)
            (message "  - %s" (plist-get result :name)))))
    (message "\n🎉 ALL TESTS PASSED!"))

  ;; Return results for programmatic use
  (list :passed greger-tree-sitter-tests-passed
        :failed greger-tree-sitter-tests-failed
        :results (reverse greger-tree-sitter-test-results)))

;; Run tests automatically when loaded
(if (treesit-ready-p 'greger)
    (greger-tree-sitter-run-all-tests)
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
