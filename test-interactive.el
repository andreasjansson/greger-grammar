#!/usr/bin/env emacs --script
;;; test-interactive.el --- Test script to reproduce segfault with file

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Create a test file
(with-temp-file "test.grgfoo"
  (insert "## USER:\nHello")
  (newline))

;; Now try to open it and activate the mode
(find-file "test.grgfoo")
(message "File opened successfully")
(message "Major mode: %s" major-mode)
(message "Buffer content: %s" (buffer-string))

;; Try to insert a newline which seems to trigger the issue
(goto-char (point-max))
(newline)
(message "Newline inserted successfully")

(message "Test completed without segfault")
