#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  (message "Mode activated, buffer size: %d" (buffer-size))

  (message "Running font-lock...")
  (font-lock-ensure)
  (message "Font-lock completed")

  (message "Buffer content after font-lock:")
  (message "%s" (buffer-substring 1 (min 200 (buffer-size)))))
