#!/usr/bin/env emacs --batch

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

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

  (message "=== BUFFER CONTENT ===")
  (message "%S" (buffer-string))

  (message "\n=== TEXT PROPERTIES DEBUG ===")
  (debug-text-properties (point-min) (point-max))

  (message "\n=== VISIBLE TEXT ===")
  (message "%S" (buffer-visible-text)))
