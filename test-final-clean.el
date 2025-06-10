#!/usr/bin/env emacs --batch

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

(ert-deftest test-complete-citation-folding-workflow ()
  "Test the complete citation folding workflow."
  (with-temp-buffer
    (insert-file-contents "test-folding.greger")
    (grgfoo-mode)
    (font-lock-ensure)

    (let ((initial (buffer-visible-text)))
      ;; Initially, citations should be folded
      (should (string-match-p "Einstein developed the theory of relativity" initial))
      (should (string-match-p "### https://physics.com/einstein" initial))
      (should-not (string-match-p "Title: Einstein Biography" initial))
      (should (string-match-p "\\[\\+2 citations, TAB to expand\\]" initial))

      ;; Expand first citation
      (goto-char (point-min))
      (search-forward "### https://physics.com/einstein")
      (beginning-of-line)
      (grgfoo-toggle-citation-fold)
      (font-lock-flush (point-min) (point-max))

      (let ((after-expand (buffer-visible-text)))
        ;; Should now see citation details
        (should (string-match-p "Title: Einstein Biography" after-expand))
        (should (string-match-p "Cited text: Albert Einstein" after-expand))
        (should (> (length after-expand) (length initial)))

        ;; Collapse it again
        (grgfoo-toggle-citation-fold)
        (font-lock-flush (point-min) (point-max))

        (let ((after-collapse (buffer-visible-text)))
          ;; Should be back to original state
          (should-not (string-match-p "Title: Einstein Biography" after-collapse))
          (should (< (length after-collapse) (length after-expand))))))))

;; Run the test
(ert-run-tests-batch-and-exit)
