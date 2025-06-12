;;; grgfoo.el --- Major mode for Greger files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Generated
;; Keywords: languages, tree-sitter
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides a major mode for Greger files with syntax highlighting,
;; navigation, citation folding, and other features powered by tree-sitter.

;;; Code:

(require 'treesit)

(defgroup grgfoo nil
  "Major mode for Greger files."
  :group 'languages
  :prefix "grgfoo-")


;;;###autoload
(define-derived-mode grgfoo-mode prog-mode "Greger"
  "Major mode for editing Greger files with tree-sitter support."
  (add-to-list 'treesit-extra-load-path default-directory)
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter for Greger isn't available"))
  (treesit-parser-create 'greger)

  (setq-local treesit-font-lock-settings greger-ui--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((error)
                (headers folding tool-folding fields)
                (tool-tags comments)
                (subheadings)))

  (setq-local treesit-simple-indent-rules greger-ui--treesit-indent-rules)
  (setq-local treesit-defun-prefer-top-level t)

  ;; Disabled because this crashes Emacs.
  ;; Reproduce: At beginning of buffer, run (treesit-search-forward-goto (treesit-node-at (point)) "" t t t)
  ;; (setq-local treesit-defun-type-regexp (rx line-start (or "user" "assistant") line-end))

  (use-local-map greger-mode-map)
  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.greger\\'" . grgfoo-mode))

(provide 'grgfoo)

;;; grgfoo.el ends here
