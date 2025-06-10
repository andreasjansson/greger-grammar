#!/usr/bin/env emacs --batch

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

(defmacro with-citation-test-buffer (&rest body)
  "Execute BODY with a test buffer containing citation content."
  `(with-temp-buffer
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
     ,@body))

(ert-deftest test-citation-expansion-debug ()
  "Debug test for citation expansion."
  (with-citation-test-buffer
    (let ((initial (buffer-visible-text)))
      (message "INITIAL STATE:\n%s" initial)

      ;; Find and click on first citation URL (which should be underlined)
      (goto-char (point-min))
      (search-forward "### https://physics.com/einstein" nil t)
      (beginning-of-line)
      (message "Clicking TAB at position %d (char: %c)" (point) (char-after))

      ;; Debug what node we're on
      (let ((node (treesit-node-at (point))))
        (when node
          (message "Current node type: %s" (treesit-node-type node))
          (let ((parent (treesit-node-parent node)))
            (when parent
              (message "Parent node type: %s" (treesit-node-type parent))))))

      ;; Check text properties at current position
      (message "Text properties at point: %s" (text-properties-at (point)))

      ;; Try to expand citation
      (grgfoo-toggle-citation-fold)
      (font-lock-flush (point-min) (point-max))

      (let ((expanded (buffer-visible-text)))
        (message "AFTER EXPANSION:\n%s" expanded)
        (message "Length change: %d -> %d" (length initial) (length expanded))))))

;; Run the test
(ert-run-tests-batch-and-exit)
