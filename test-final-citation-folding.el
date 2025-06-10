#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

;; Disable debug messages for cleaner output
(setq grgfoo-citation-folding-enabled t)

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  (font-lock-ensure)

  (message "=== DEFAULT VIEW (citations folded) ===")
  (let ((visible (buffer-visible-text)))
    (message "%s" visible))

  (message "\n=== EXPANDING FIRST CITATION ===")
  (goto-char 75)  ; Position in first citation text
  (grgfoo-toggle-citation-fold)
  (font-lock-flush (point-min) (point-max))

  (let ((visible (buffer-visible-text)))
    (message "%s" (substring visible 0 (min 400 (length visible)))))

  (message "\n=== EXPANDING CITATIONS SECTION ===")
  (goto-char (point-min))
  (search-forward "## CITATIONS:" nil t)
  (beginning-of-line)
  (grgfoo-toggle-citation-fold)
  (font-lock-flush (point-min) (point-max))

  (let ((visible (buffer-visible-text)))
    (message "%s" visible))

  (message "\nCitation folding test completed successfully!"))
