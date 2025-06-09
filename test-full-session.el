#!/usr/bin/env emacs --script
;;; test-full-session.el --- Test a full user session

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Clean up any existing test file
(when (file-exists-p "full-test.grgfoo")
  (delete-file "full-test.grgfoo"))

;; Create and open the file
(find-file "full-test.grgfoo")
(message "=== Created new .grgfoo file ===")
(message "Major mode: %s" major-mode)

;; Test the scenario described: create a new file, type content, hit return
(insert "## USER:")
(message "Inserted ## USER:")

(newline)
(message "Hit return after header - no crash!")

(insert "What is the meaning of life?")
(message "Added question")

(newline)
(message "Hit return after content - no crash!")

(newline)
(insert "## ASSISTANT:")
(newline)
(insert "The meaning of life is a profound philosophical question.")
(message "Added assistant response - no crash!")

;; Test some tree-sitter features
(goto-char (point-min))
(message "=== Testing navigation ===")

;; Test defun navigation
(condition-case err
    (progn
      (beginning-of-defun)
      (message "beginning-of-defun worked")
      (end-of-defun)
      (message "end-of-defun worked"))
  (error (message "Navigation error: %s" err)))

;; Test indentation
(condition-case err
    (progn
      (goto-char (point-max))
      (newline)
      (insert "    This should be indented properly")
      (indent-for-tab-command)
      (message "Indentation worked"))
  (error (message "Indentation error: %s" err)))

;; Save the buffer
(save-buffer)
(message "=== Buffer saved successfully ===")

;; Show final content
(message "Final buffer content:")
(message "%s" (buffer-string))

(kill-buffer)
(message "=== Session completed without any crashes ===")

;; Clean up
(when (file-exists-p "full-test.grgfoo")
  (delete-file "full-test.grgfoo"))
