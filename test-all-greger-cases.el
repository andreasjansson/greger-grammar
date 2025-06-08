;;; Comprehensive test for greger-tree-sitter -*- lexical-binding: t -*-

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

(defvar greger-tree-sitter-test-results '())
(defvar greger-tree-sitter-tests-passed 0)
(defvar greger-tree-sitter-tests-failed 0)

(defun greger-tree-sitter-test-equal (expected actual)
  "Compare two dialog structures for equality."
  (equal expected actual))

(defun greger-tree-sitter-run-single-test (name markdown expected)
  "Run a single test case and return result."
  (let ((start-time (current-time)))
    (message "\n=== Testing: %s ===" name)

    (condition-case err
        (let* ((actual (greger-tree-sitter-parse markdown))
               (elapsed (float-time (time-subtract (current-time) start-time))))

          ;; Check if results match
          (if (greger-tree-sitter-test-equal expected actual)
              (progn
                (message "✅ PASSED (%.3fs)" elapsed)
                (setq greger-tree-sitter-tests-passed (1+ greger-tree-sitter-tests-passed)))
            (progn
              (message "❌ FAILED (%.3fs)" elapsed)
              (message "\nExpected:")
              (pp expected)
              (message "\nActual:")
              (pp actual)
              (setq greger-tree-sitter-tests-failed (1+ greger-tree-sitter-tests-failed)))))

      (error
       (message "❌ ERROR: %s" (error-message-string err))
       (setq greger-tree-sitter-tests-failed (1+ greger-tree-sitter-tests-failed))))))

(defun greger-tree-sitter-run-all-tests ()
  "Run all test cases and report results."
  (message "\n🧪 Running greger-tree-sitter comprehensive tests...")

  ;; Reset counters
  (setq greger-tree-sitter-tests-passed 0)
  (setq greger-tree-sitter-tests-failed 0)

  ;; Check if tree-sitter is available
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  ;; Run individual tests

  ;; Simple user message
  (greger-tree-sitter-run-single-test
   "simple-user-message"
   (greger-read-corpus-file "simple-user-message")
   '(((role . "user")
      (content . "Hello, how are you?"))))

  ;; Tool use with single parameter
  (greger-tree-sitter-run-single-test
   "tool-use-single-param"
   (greger-read-corpus-file "tool-use-single-param")
   '(((role . "user")
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

  ;; Thinking only
  (greger-tree-sitter-run-single-test
   "thinking-only"
   (greger-read-corpus-file "thinking-only")
   '(((role . "user")
      (content . "Let me think about this"))
     ((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . "I need to consider all the options carefully before responding.")))))))

  ;; Report summary
  (message "\n📊 TEST SUMMARY:")
  (message "Total tests: %d" (+ greger-tree-sitter-tests-passed greger-tree-sitter-tests-failed))
  (message "Passed: %d" greger-tree-sitter-tests-passed)
  (message "Failed: %d" greger-tree-sitter-tests-failed)

  (if (> greger-tree-sitter-tests-failed 0)
      (message "\n❌ Some tests failed")
    (message "\n🎉 ALL TESTS PASSED!")))

;; Run tests automatically when loaded
(if (treesit-ready-p 'greger)
    (greger-tree-sitter-run-all-tests)
  (message "❌ Tree-sitter greger parser not available"))

(provide 'test-all-greger-cases)
