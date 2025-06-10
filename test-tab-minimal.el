#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  (font-lock-ensure)

  (message "Testing TAB at position 1...")
  (goto-char 1)
  (condition-case err
      (grgfoo-toggle-citation-fold)
    (error (message "ERROR: %s" err)))

  (message "TAB test completed"))
