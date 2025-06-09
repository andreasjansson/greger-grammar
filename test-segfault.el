#!/usr/bin/env emacs --script
;;; test-segfault.el --- Test script to reproduce segfault

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Create a test buffer with .grgfoo content
(with-temp-buffer
  (insert "## USER:\nHello")
  (newline)
  (grgfoo-mode)
  (message "Mode activated successfully")
  (message "Buffer content: %s" (buffer-string))
  (message "Major mode: %s" major-mode))

(message "Test completed without segfault")
