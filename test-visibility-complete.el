#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")
(load-file "test-visibility-utils.el")

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  (font-lock-ensure)

  (message "=== INITIAL STATE (folded) ===")
  (let ((visible (buffer-visible-text)))
    (message "Visible text length: %d" (length visible))
    (message "First 200 chars: %s" (substring visible 0 (min 200 (length visible))))
    (message "Contains 'Title: Einstein'? %s" (if (string-match-p "Title: Einstein" visible) "YES" "NO"))
    (message "Contains 'Cited text:'? %s" (if (string-match-p "Cited text:" visible) "YES" "NO")))

  (message "\n=== EXPANDING FIRST CITATION ===")
  (goto-char 61)  ; First citation
  (grgfoo-toggle-citation-fold)
  (font-lock-flush (point-min) (point-max))

  (let ((visible (buffer-visible-text)))
    (message "Visible text length: %d" (length visible))
    (message "First 300 chars: %s" (substring visible 0 (min 300 (length visible))))
    (message "Contains 'Title: Einstein'? %s" (if (string-match-p "Title: Einstein" visible) "YES" "NO"))
    (message "Contains 'Cited text:'? %s" (if (string-match-p "Cited text:" visible) "YES" "NO")))

  (message "\nTest completed"))
