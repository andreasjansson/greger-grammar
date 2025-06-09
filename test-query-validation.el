#!/usr/bin/env emacs --script
;;; test-query-validation.el --- Validate tree-sitter queries

(setq debug-on-error t)

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Test the problematic query
(when (treesit-ready-p 'greger)
  (message "Testing tree-sitter queries...")

  ;; Test each query rule separately
  (let ((queries '(
    ;; Basic comment query
    ((html_comment) @font-lock-comment-face)

    ;; Simple heading query
    ((user) @font-lock-function-name-face)

    ;; Value query that seems to cause issues
    ((value) @font-lock-string-face)

    ;; Citation URL query
    ((citation_url) @font-lock-constant-face)
    )))

    (dolist (query queries)
      (condition-case err
          (progn
            (treesit-query-validate 'greger query)
            (message "Query valid: %s" query))
        (error (message "Query invalid: %s - Error: %s" query err)))))

  ;; Create a simple test buffer to see what nodes are actually generated
  (with-temp-buffer
    (insert "## USER:\nHello world\n")
    (let ((parser (treesit-parser-create 'greger)))
      (treesit-parser-add-notifier parser (lambda (&rest _) nil))
      (let ((root (treesit-parser-root-node parser)))
        (message "Root node type: %s" (treesit-node-type root))
        (message "Root node text: %s" (treesit-node-text root))

        ;; Print all child nodes
        (let ((child-count (treesit-node-child-count root)))
          (message "Child count: %d" child-count)
          (dotimes (i child-count)
            (let ((child (treesit-node-child root i)))
              (when child
                (message "Child %d: type=%s text=%s"
                         i
                         (treesit-node-type child)
                         (treesit-node-text child))))))))))

(message "Query validation completed")
