#!/usr/bin/env emacs --batch

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

(ert-deftest test-tab-functionality ()
  "Test that TAB key can expand/collapse citations."
  (with-temp-buffer
    (insert "## ASSISTANT:

Einstein developed the theory of relativity

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

## CITATIONS:

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456
")

    (grgfoo-mode)
    (font-lock-ensure)

    ;; Test initial folded state
    (let ((initial-visible (buffer-visible-text)))
      (should (string-match-p "Einstein developed the theory of relativity" initial-visible))
      (should (string-match-p "\\[\\+1 citation" initial-visible)))

    ;; Try to expand citations section
    (goto-char (point-min))
    (re-search-forward "## CITATIONS:")
    (end-of-line)
    (grgfoo-toggle-citation-fold)

    ;; Check if citations section expanded
    (let ((expanded-visible (buffer-visible-text)))
      (should (string-match-p "Einstein developed the theory of relativity" expanded-visible))
      ;; Should now show the actual citation content
      (should (string-match-p "https://physics.com/einstein" expanded-visible)))))

;; Run the tests
(ert-run-tests-batch-and-exit)
