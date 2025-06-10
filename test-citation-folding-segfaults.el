#!/usr/bin/env emacs --batch

;; Test for segfaults in citation folding
;; Similar to test-major-mode-segfaults.el

(setq debug-on-error t)
(setq debug-on-segfault t)

(add-to-list 'treesit-extra-load-path default-directory)

;; Load the mode
(load-file "grgfoo.el")

(defun test-citation-folding-segfault ()
  "Test various operations that might cause segfaults."
  (with-temp-buffer
    (insert-file-contents "test-folding.greger")
    (grgfoo-mode)

    (message "Testing basic font-lock...")
    (font-lock-ensure)
    (message "Font-lock completed")

    ;; Test TAB on various positions that might cause segfaults
    (message "Testing TAB at different positions...")

    ;; Test TAB on first assistant header
    (goto-char (point-min))
    (search-forward "## ASSISTANT:" nil t)
    (beginning-of-line)
    (message "Testing TAB on first ASSISTANT header at pos %d" (point))
    (condition-case err
        (grgfoo-toggle-citation-fold)
      (error (message "Error on first ASSISTANT: %s" err)))

    ;; Test TAB on "Einstein developed" text
    (goto-char (point-min))
    (search-forward "Einstein developed" nil t)
    (message "Testing TAB on 'Einstein developed' at pos %d" (point))
    (condition-case err
        (grgfoo-toggle-citation-fold)
      (error (message "Error on Einstein text: %s" err)))

    ;; Test TAB on "while" text
    (goto-char (point-min))
    (search-forward "while" nil t)
    (message "Testing TAB on 'while' at pos %d" (point))
    (condition-case err
        (grgfoo-toggle-citation-fold)
      (error (message "Error on 'while' text: %s" err)))

    ;; Test TAB on second assistant header
    (goto-char (point-min))
    (search-forward "## ASSISTANT:" nil t)
    (search-forward "## ASSISTANT:" nil t)
    (beginning-of-line)
    (message "Testing TAB on second ASSISTANT header at pos %d" (point))
    (condition-case err
        (grgfoo-toggle-citation-fold)
      (error (message "Error on second ASSISTANT: %s" err)))

    ;; Test TAB on citations header
    (goto-char (point-min))
    (search-forward "## CITATIONS:" nil t)
    (beginning-of-line)
    (message "Testing TAB on CITATIONS header at pos %d" (point))
    (condition-case err
        (grgfoo-toggle-citation-fold)
      (error (message "Error on CITATIONS header: %s" err)))

    (message "All TAB tests completed without segfault")))

;; Run the test
(test-citation-folding-segfault)
(message "Segfault test completed successfully")
