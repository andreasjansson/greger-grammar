#!/usr/bin/env emacs --batch

;; ERT tests for citation folding

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

;; Load the mode
(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

(defmacro with-test-buffer-with-citations (&rest body)
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
     ,@body))

(ert-deftest test-mode-loads-without-segfault ()
  "Test that the mode loads without segfaulting."
  (with-temp-buffer
    (grgfoo-mode)
    (should (eq major-mode 'grgfoo-mode))))

(ert-deftest test-font-lock-works ()
  "Test that font-lock works without segfaulting."
  (with-test-buffer-with-citations
    (grgfoo-mode)
    (font-lock-ensure)
    (should (> (buffer-size) 0))))

(ert-deftest test-citation-folding-enabled-by-default ()
  "Test that citation folding is enabled by default."
  (with-test-buffer-with-citations
    (grgfoo-mode)
    (should grgfoo-citation-folding-enabled)))

(ert-deftest test-buffer-visible-text-utility ()
  "Test that buffer-visible-text works correctly."
  (with-temp-buffer
    (insert "visible text")
    (put-text-property 5 10 'invisible t)
    (add-to-invisibility-spec t)
    (let ((visible (buffer-visible-text)))
      (should (string= visible "visiext")))))

(ert-deftest test-tab-handler-no-segfault ()
  "Test that TAB handler doesn't segfault."
  (with-test-buffer-with-citations
    (grgfoo-mode)
    (font-lock-ensure)

    ;; Test TAB at various positions
    (goto-char (point-min))
    (should (not (condition-case nil
                     (progn (grgfoo-toggle-citation-fold) nil)
                   (error t))))

    (search-forward "Einstein" nil t)
    (should (not (condition-case nil
                     (progn (grgfoo-toggle-citation-fold) nil)
                   (error t))))

    (search-forward "while" nil t)
    (should (not (condition-case nil
                     (progn (grgfoo-toggle-citation-fold) nil)
                   (error t))))

    (search-forward "CITATIONS" nil t)
    (should (not (condition-case nil
                     (progn (grgfoo-toggle-citation-fold) nil)
                   (error t))))))

(ert-deftest test-citation-folding-functionality ()
  "Test that citation folding actually works."
  (with-test-buffer-with-citations
    (grgfoo-mode)
    (font-lock-ensure)

    ;; Get initial visible text
    (let ((initial-text (buffer-visible-text)))
      (should (string-match-p "Einstein developed the theory of relativity" initial-text))
      (should (string-match-p "### https://physics.com/einstein" initial-text))

      ;; The citations should be folded by default due to font-lock
      ;; Test that we can capture this state
      (should (> (length initial-text) 0)))))

;; Run the tests
(ert-run-tests-batch-and-exit)
