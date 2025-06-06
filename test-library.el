;;; test-library.el --- Test if tree-sitter library is loadable -*- lexical-binding: t -*-

;; First, add current directory to the treesit load path
(add-to-list 'treesit-extra-load-path default-directory)

(defun test-greger-library ()
  "Test if the greger tree-sitter library can be loaded."
  (interactive)
  (message "Current directory: %s" default-directory)
  (message "treesit-extra-load-path: %s" treesit-extra-load-path)

  ;; Check what library files exist
  (let ((lib-files (directory-files default-directory nil ".*greger.*\\(so\\|dylib\\|dll\\)$")))
    (message "Found library files: %s" lib-files))

  ;; Test if treesit can find and load the language
  (condition-case err
      (progn
        (message "Testing treesit-ready-p...")
        (if (treesit-ready-p 'greger)
            (message "✅ SUCCESS: Tree-sitter greger parser is ready!")
          (message "❌ FAILED: Tree-sitter greger parser not ready"))

        ;; Try to create a parser
        (message "Testing parser creation...")
        (with-temp-buffer
          (treesit-parser-create 'greger)
          (message "✅ SUCCESS: Parser created successfully!")))
    (error
     (message "❌ ERROR: %s" err))))

;; Run the test
(test-greger-library)
