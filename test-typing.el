#!/usr/bin/env emacs --script
;;; test-typing.el --- Test typing scenario

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Clean up any existing test file
(when (file-exists-p "test.grgfoo")
  (delete-file "test.grgfoo"))

;; Create and open the file
(find-file "test.grgfoo")
(message "File created and opened")

;; Start typing content
(insert "## USER:\n")
(message "Inserted header")

(insert "Hello world")
(message "Inserted content")

;; Hit return (this is where the segfault supposedly occurs)
(newline)
(message "Inserted newline - no segfault!")

;; Try some more operations
(insert "More content")
(message "Inserted more content")

(newline)
(insert "## ASSISTANT:\n")
(insert "Response here")
(message "Added assistant response")

;; Save and close
(save-buffer)
(kill-buffer)

(message "Test completed successfully - no segfault detected")

;; Clean up
(when (file-exists-p "test.grgfoo")
  (delete-file "test.grgfoo"))
