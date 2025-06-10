#!/usr/bin/env emacs --batch

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

(defun get-visible-buffer (buffer)
  "Get the visible text from BUFFER as you would see it on screen."
  (with-current-buffer buffer
    (buffer-visible-text)))

(ert-deftest test-citations-folding ()
  "Test that citations fold correctly and show the expected exact text."
  (with-temp-buffer
    (insert "## ASSISTANT:\n\n")
    (insert "Einstein developed the theory of relativity\n\n")
    (insert "### https://physics.com/einstein\n\n")
    (insert "Title: Einstein Biography\n")
    (insert "Cited text: Albert Einstein developed the theory of relativity in the early 20th century...\n")
    (insert "Encrypted index: def456\n\n")
    (insert "## ASSISTANT:\n\n")
    (insert "while Newton formulated the laws of motion\n\n")
    (insert "### https://physics.com/newton\n\n")
    (insert "Title: Newton Biography\n")
    (insert "Cited text: Isaac Newton formulated the three laws of motion...\n")
    (insert "Encrypted index: ghi789\n\n")
    (insert "## CITATIONS:\n\n")
    (insert "### https://physics.com/einstein\n\n")
    (insert "Title: Einstein Biography\n")
    (insert "Cited text: Albert Einstein developed the theory of relativity in the early 20th century...\n")
    (insert "Encrypted index: def456\n\n")
    (insert "### https://physics.com/newton\n\n")
    (insert "Title: Newton Biography\n")
    (insert "Cited text: Isaac Newton formulated the three laws of motion...\n")
    (insert "Encrypted index: ghi789\n")

    (grgfoo-mode)
    (font-lock-ensure)

    (let ((actual (get-visible-buffer (current-buffer)))
          (expected "## ASSISTANT:\n\nEinstein developed the theory of relativity\n\n## ASSISTANT:\n\nwhile Newton formulated the laws of motion\n\n## CITATIONS:\n\n[+2 citations, TAB to expand]\n"))

      (message "=== ACTUAL OUTPUT ===")
      (message "%S" actual)
      (message "\n=== EXPECTED OUTPUT ===")
      (message "%S" expected)
      (message "\n=== ACTUAL LENGTH: %d, EXPECTED LENGTH: %d ===" (length actual) (length expected))

      ;; Show character-by-character differences
      (message "\n=== CHARACTER BY CHARACTER ===")
      (dotimes (i (max (length actual) (length expected)))
        (let ((a-char (if (< i (length actual)) (aref actual i) 'END))
              (e-char (if (< i (length expected)) (aref expected i) 'END)))
          (when (not (equal a-char e-char))
            (message "Position %d: actual=%s expected=%s" i a-char e-char))))

      (should (string= expected actual)))))

;; Run the tests
(ert-run-tests-batch-and-exit)
