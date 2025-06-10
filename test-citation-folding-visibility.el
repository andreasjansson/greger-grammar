#!/usr/bin/env emacs --batch

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

;; Load the mode and visibility utilities
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

(ert-deftest test-citations-folding-default-view ()
  "Test that citations are folded by default and show expected text."
  (with-citation-test-buffer
    (let ((actual (buffer-visible-text))
          (expected "## ASSISTANT:\n\nEinstein developed the theory of relativity while Newton formulated the laws of motion\n\n## CITATIONS:\n[+2 citations, TAB to expand]\n"))
      (message "ACTUAL OUTPUT:\n%s" actual)
      (message "EXPECTED OUTPUT:\n%s" expected)
      (message "ACTUAL LENGTH: %d, EXPECTED LENGTH: %d" (length actual) (length expected))

      ;; Check that individual citation details are hidden
      (should-not (string-match-p "Title: Einstein Biography" actual))
      (should-not (string-match-p "Cited text: Albert Einstein" actual))
      (should-not (string-match-p "Encrypted index: def456" actual))

      ;; Check that main text is visible
      (should (string-match-p "Einstein developed the theory of relativity" actual))
      (should (string-match-p "Newton formulated the laws of motion" actual))
      (should (string-match-p "## CITATIONS:" actual)))))

(ert-deftest test-citation-expansion ()
  "Test that expanding a citation shows the details."
  (with-citation-test-buffer
    (let ((initial (buffer-visible-text)))
      (message "INITIAL STATE:\n%s" initial)

      ;; Find and click on first citation URL (which should be underlined)
      (goto-char (point-min))
      (search-forward "### https://physics.com/einstein" nil t)
      (beginning-of-line)
      (message "Clicking TAB at position %d (char: %c)" (point) (char-after))

      ;; Try to expand citation
      (grgfoo-toggle-citation-fold)
      (font-lock-flush (point-min) (point-max))

      (let ((expanded (buffer-visible-text)))
        (message "AFTER EXPANSION:\n%s" expanded)

        ;; After expansion, should see citation details
        (should (string-match-p "Title: Einstein Biography" expanded))
        (should (string-match-p "Cited text: Albert Einstein" expanded))))))

(ert-deftest test-citations-section-expansion ()
  "Test that expanding the citations section shows all citations."
  (with-citation-test-buffer
    (let ((initial (buffer-visible-text)))
      (message "INITIAL STATE:\n%s" initial)

      ;; Find and click on citations header
      (goto-char (point-min))
      (search-forward "## CITATIONS:" nil t)
      (beginning-of-line)
      (message "Clicking TAB on CITATIONS header at position %d" (point))

      ;; Try to expand citations section
      (grgfoo-toggle-citation-fold)
      (font-lock-flush (point-min) (point-max))

      (let ((expanded (buffer-visible-text)))
        (message "AFTER CITATIONS EXPANSION:\n%s" expanded)

        ;; After expansion, should see all citation details in citations section
        (should (>= (length expanded) (length initial)))
        (should (string-match-p "### https://physics.com/einstein" expanded))
        (should (string-match-p "### https://physics.com/newton" expanded))))))

(ert-deftest test-debug-text-properties ()
  "Debug test to check what text properties are actually applied."
  (with-citation-test-buffer
    (message "=== DEBUGGING TEXT PROPERTIES ===")

    ;; Check properties at different positions
    (goto-char (point-min))
    (search-forward "Einstein developed" nil t)
    (let ((pos (point)))
      (message "Position %d (Einstein text): %s" pos (text-properties-at pos)))

    (goto-char (point-min))
    (search-forward "### https://physics.com/einstein" nil t)
    (let ((pos (point)))
      (message "Position %d (first URL): %s" pos (text-properties-at pos)))

    (goto-char (point-min))
    (search-forward "Title: Einstein" nil t)
    (let ((pos (point)))
      (message "Position %d (Title): %s" pos (text-properties-at pos)))

    (goto-char (point-min))
    (search-forward "## CITATIONS:" nil t)
    (let ((pos (point)))
      (message "Position %d (Citations header): %s" pos (text-properties-at pos)))

    ;; Debug the tree structure
    (goto-char (point-min))
    (search-forward "### https://physics.com/einstein" nil t)
    (let ((node (treesit-node-at (point))))
      (when node
        (message "Node at first URL: type=%s start=%d end=%d"
                 (treesit-node-type node)
                 (treesit-node-start node)
                 (treesit-node-end node))
        (let ((parent (treesit-node-parent node)))
          (when parent
            (message "Parent node: type=%s start=%d end=%d"
                     (treesit-node-type parent)
                     (treesit-node-start parent)
                     (treesit-node-end parent))))))))

;; Run the tests
(ert-run-tests-batch-and-exit)
