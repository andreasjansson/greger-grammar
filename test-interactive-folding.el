#!/usr/bin/env emacs

;; Interactive test for citation folding
;; Run this with: emacs -l test-interactive-folding.el

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

;; Load the grammar and mode
(load-file (expand-file-name "grgfoo.el"))

;; Open the test file
(find-file (expand-file-name "test-folding.greger"))

;; Switch to grgfoo-mode
(grgfoo-mode)

;; Enable font-lock
(font-lock-mode 1)

;; Position cursor at the first citation
(goto-char (point-min))
(search-forward "Einstein developed" nil t)
(beginning-of-line)

;; Display instructions
(message "Citation folding test loaded!")
(with-output-to-temp-buffer "*Citation Folding Instructions*"
  (princ "CITATION FOLDING TEST

This buffer contains a Greger file with citation folding enabled.

Instructions:
1. Position your cursor on the underlined text 'Einstein developed the theory of relativity'
2. Press TAB to expand/collapse that citation
3. Position your cursor on '## CITATIONS:' header
4. Press TAB to expand/collapse the entire citations section
5. Try moving between different citation texts and toggling them

Note: If text doesn't appear underlined, folding might not be working.
Check that tree-sitter is loaded and the grammar is available.

Current position: Point is positioned at the first citation text.
Press TAB now to test folding!"))
