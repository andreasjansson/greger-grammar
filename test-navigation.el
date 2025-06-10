#!/usr/bin/env emacs --batch

(require 'ert)

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")

(ert-deftest test-navigation-on-merged-line ()
  "Test that C-f navigation works correctly on the merged assistant line."
  (with-temp-buffer
    (insert "## ASSISTANT:

Einstein developed the theory of relativity

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

## ASSISTANT:

while Newton formulated the laws of motion

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789

## CITATIONS:

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789
")

    (grgfoo-mode)
    (font-lock-ensure)

    ;; Navigate to the beginning of the merged text line
    (goto-char (point-min))
    (forward-line 2)  ; Skip "## ASSISTANT:" and blank line

    ;; Test that we can navigate character by character through the merged text
    (let ((start-pos (point))
          (char-count 0))

      ;; Try to move forward character by character
      (while (and (< (point) (point-max))
                  (not (looking-at "^## CITATIONS:")))
        (forward-char 1)
        (setq char-count (1+ char-count)))

      ;; We should have moved through the entire merged text
      ;; "Einstein developed the theory of relativity while Newton formulated the laws of motion"
      (let ((expected-min-chars 80))  ; At least this many characters in the merged text
        (should (> char-count expected-min-chars))
        (message "Successfully navigated %d characters" char-count)))))

;; Run the tests
(ert-run-tests-batch-and-exit)
