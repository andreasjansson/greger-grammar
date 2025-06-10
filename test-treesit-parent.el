#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

(load-file "grgfoo.el")

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  (font-lock-ensure)

  (message "Getting node at position 1...")
  (goto-char 1)
  (let ((node (treesit-node-at (point))))
    (message "Node type: %s" (treesit-node-type node))

    (message "Getting parent...")
    (let ((parent (treesit-node-parent node)))
      (if parent
          (message "Parent type: %s" (treesit-node-type parent))
        (message "No parent"))

      (when parent
        (message "Getting grandparent...")
        (let ((grandparent (treesit-node-parent parent)))
          (if grandparent
              (message "Grandparent type: %s" (treesit-node-type grandparent))
            (message "No grandparent"))))))

  (message "Test completed"))
