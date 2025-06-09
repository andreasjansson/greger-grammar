;;; grgfoo.el --- Major mode for Greger files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Generated
;; Keywords: languages, tree-sitter
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides a major mode for Greger files with syntax highlighting,
;; navigation, and other features powered by tree-sitter.

;;; Code:

(require 'treesit)

(defcustom grgfoo-ts-indent-offset 2
  "Number of spaces for each indentation step in `grgfoo-mode'."
  :type 'integer
  :safe 'integerp
  :group 'grgfoo)

(defvar grgfoo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "'" table)
    ;; Brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Angle brackets for HTML tags
    (modify-syntax-entry ?\< "(>" table)
    (modify-syntax-entry ?\> ")<" table)
    table)
  "Syntax table for `grgfoo-mode'.")

(defvar grgfoo--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'greger
   :feature 'heading
   :override t
   '(;; Major section headings
     (user) @font-lock-function-name-face
     (assistant) @font-lock-function-name-face
     (system) @font-lock-function-name-face
     (thinking) @font-lock-function-name-face
     (tool_use) @font-lock-function-name-face
     (tool_result) @font-lock-function-name-face
     (server_tool_use) @font-lock-function-name-face
     (web_search_tool_result) @font-lock-function-name-face
     (citations) @font-lock-function-name-face)

   :language 'greger
   :feature 'field
   :override t
   '(;; Field names and values
     (name) @font-lock-builtin-face
     (id) @font-lock-builtin-face
     (value) @font-lock-string-face
     (url) @font-lock-constant-face)

   :language 'greger
   :feature 'subheading
   :override t
   '(;; Sub-sections
     (tool_param) @font-lock-keyword-face
     (citation_entry) @font-lock-keyword-face)

   :language 'greger
   :feature 'content
   :override t
   '(;; Content and text
     (text) @default
     (html_comment) @font-lock-comment-face)

   :language 'greger
   :feature 'error
   :override t
   '(;; Parse errors
     (ERROR) @grgfoo-error-face))
  "Tree-sitter font-lock settings for `grgfoo-mode'.")

(defface grgfoo-error-face
  '((t (:background "red" :foreground "white")))
  "Face for parse errors in grgfoo-mode."
  :group 'grgfoo)

(defvar grgfoo--treesit-indent-rules
  `((greger
     ;; Safe indentation rules that won't cause segfaults
     ((parent-is "source_file") column-0 0)
     ((node-is "text") parent-bol 0)
     ((node-is "value") parent-bol ,grgfoo-ts-indent-offset)
     (catch-all parent-bol 0)))
  "Tree-sitter indentation rules for `grgfoo-mode'.")

;;;###autoload
(define-derived-mode grgfoo-mode prog-mode "Greger"
  "Major mode for editing Greger files with tree-sitter support.

\\{grgfoo-mode-map}"
  :syntax-table grgfoo-mode-syntax-table

  (when (treesit-ready-p 'greger)
    ;; Create the tree-sitter parser for this buffer
    (treesit-parser-create 'greger)

    ;; Tree-sitter setup
    (setq-local treesit-font-lock-settings grgfoo--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((error)                    ; level 1 - always show errors
                  (heading)                  ; level 2 - basic structure
                  (field subheading)         ; level 3 - detailed structure
                  (content)))

    ;; Indentation - using safe rules
    (setq-local treesit-simple-indent-rules grgfoo--treesit-indent-rules)

    ;; Navigation - treat headings as defuns for C-M-a and C-M-e
    (setq-local treesit-defun-type-regexp
                (rx (or "user" "assistant" "system" "thinking"
                        "tool_use" "tool_result" "server_tool_use"
                        "web_search_tool_result" "citations")))

    ;; Set up defun name function to show heading type
    (setq-local treesit-defun-name-function #'grgfoo--defun-name)

    ;; Enable all tree-sitter features
    (treesit-major-mode-setup)))



(defun grgfoo--defun-name (node)
  "Return the name of the defun NODE."
  (when (and node (treesit-node-p node))
    (condition-case nil
        (pcase (treesit-node-type node)
          ("user" "USER")
          ("assistant" "ASSISTANT")
          ("system" "SYSTEM")
          ("thinking" "THINKING")
          ("tool_use" "TOOL USE")
          ("tool_result" "TOOL RESULT")
          ("server_tool_use" "SERVER TOOL USE")
          ("web_search_tool_result" "WEB SEARCH TOOL RESULT")
          ("citations" "CITATIONS")
          (_ nil))
      (error nil))))

;; Ensure the grammar is loaded
(add-to-list 'treesit-extra-load-path default-directory)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grgfoo\\'" . grgfoo-mode))

(provide 'grgfoo)

;;; grgfoo.el ends here
