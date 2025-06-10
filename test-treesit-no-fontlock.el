#!/usr/bin/env emacs --batch

(setq debug-on-error t)
(add-to-list 'treesit-extra-load-path default-directory)

;; Temporarily disable citation folding
(setq grgfoo-citation-folding-enabled nil)

(load-file "grgfoo.el")

(with-temp-buffer
  (insert-file-contents "test-folding.greger")
  (grgfoo-mode)
  ;; Don't run font-lock-ensure

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
            (message "No grandparent"))

          (when grandparent
            (message "Getting great-grandparent...")
            (let ((ggparent (treesit-node-parent grandparent)))
              (if ggparent
                  (message "Great-grandparent type: %s" (treesit-node-type ggparent))
                (message "No great-grandparent"))))))))

  (message "Test completed"))
