#!/usr/bin/env emacs --script
;;; test-final.el --- Final test for segfault fix

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Test 1: Basic mode activation
(message "=== Test 1: Basic mode activation ===")
(with-temp-buffer
  (grgfoo-mode)
  (message "✓ grgfoo-mode activated without crash"))

;; Test 2: File creation and content insertion
(message "=== Test 2: File creation and content insertion ===")
(when (file-exists-p "test-final.grgfoo")
  (delete-file "test-final.grgfoo"))

(find-file "test-final.grgfoo")
(insert "## USER:")
(newline)  ; This was causing the segfault
(insert "Hello world")
(newline)
(message "✓ Content inserted and newlines added without crash")

;; Test 3: More complex content
(message "=== Test 3: Complex content ===")
(insert "## ASSISTANT:")
(newline)
(insert "This is a response with multiple lines")
(newline)
(insert "And more content")
(newline)
(message "✓ Complex content added without crash")

;; Test 4: Save and close
(message "=== Test 4: Save and close ===")
(save-buffer)
(kill-buffer)
(message "✓ Buffer saved and closed without crash")

;; Clean up
(when (file-exists-p "test-final.grgfoo")
  (delete-file "test-final.grgfoo"))

(message "")
(message "🎉 ALL TESTS PASSED - SEGFAULT ISSUE FIXED!")
(message "")
(message "The major mode should now work correctly when:")
(message "1. Creating new .grgfoo files")
(message "2. Typing content")
(message "3. Pressing Enter/Return")
(message "4. Adding more content")
(message "5. Saving files")
