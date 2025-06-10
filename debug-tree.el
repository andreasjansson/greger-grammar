#!/usr/bin/env emacs --script

(setq default-directory (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'treesit-extra-load-path default-directory)

(load-file (expand-file-name "grgfoo.el"))

(with-temp-buffer
  (insert-file-contents (expand-file-name "test-folding.greger"))
  (grgfoo-mode)
  (message "Buffer contents length: %d" (buffer-size))

  ;; Get the parse tree
  (when-let ((parser (treesit-parser-create 'greger)))
    (let ((root-node (treesit-parser-root-node parser)))
      (message "Root node type: %s" (treesit-node-type root-node))

      ;; Look for assistant nodes
      (let ((assistant-nodes (treesit-query-capture root-node '((assistant) @assistant))))
        (message "Found %d assistant nodes" (length assistant-nodes))
        (dolist (capture assistant-nodes)
          (let ((node (cdr capture)))
            (message "Assistant node at %d-%d"
                     (treesit-node-start node)
                     (treesit-node-end node))
            ;; Look at children
            (let ((children (treesit-node-children node)))
              (message "  Has %d children" (length children))
              (dolist (child children)
                (message "    Child: %s at %d-%d"
                         (treesit-node-type child)
                         (treesit-node-start child)
                         (treesit-node-end child)))))))

      ;; Look for citation nodes
      (let ((citation-nodes (treesit-query-capture root-node '((citation_entry) @citation))))
        (message "Found %d citation_entry nodes" (length citation-nodes))
        (dolist (capture citation-nodes)
          (let ((node (cdr capture)))
            (message "Citation node at %d-%d, parent: %s"
                     (treesit-node-start node)
                     (treesit-node-end node)
                     (treesit-node-type (treesit-node-parent node)))))))))
