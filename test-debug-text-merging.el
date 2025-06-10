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
")

  (grgfoo-mode)
  (font-lock-ensure)

  (message "=== DEBUG: Looking for assistant blocks ===")
  (goto-char (point-min))
  (let ((root-node (treesit-parser-root-node (treesit-parser-create 'greger))))
    (let ((assistant-nodes (treesit-query-capture root-node '((assistant) @assistant))))
      (message "Found %d assistant nodes" (length assistant-nodes))
      (dolist (capture assistant-nodes)
        (let ((node (cdr capture)))
          (message "Assistant node: start=%d end=%d"
                   (treesit-node-start node) (treesit-node-end node))
          (let ((children (treesit-node-children node)))
            (message "  Children: %d" (length children))
            (dolist (child children)
              (message "    Child: type=%s start=%d end=%d text='%s'"
                       (treesit-node-type child)
                       (treesit-node-start child)
                       (treesit-node-end child)
                       (string-trim (buffer-substring-no-properties
                                   (treesit-node-start child)
                                   (min (+ (treesit-node-start child) 20) (treesit-node-end child)))))))))))

  (message "\n=== VISIBLE TEXT ===")
  (message "%s" (buffer-visible-text)))
