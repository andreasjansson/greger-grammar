#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  (font-lock-ensure)

  (message "Testing TAB on citation text...")
  ;; Go to the underlined citation text
  (goto-char 61)  ; Start of first citation
  (message "Position %d, character: '%s'" (point) (char-after))
  (condition-case err
      (grgfoo-toggle-citation-fold)
    (error (message "ERROR: %s" err)))

  (message "TAB on citation test completed"))
