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
   :feature 'comment
   :override t
   '((html_comment) @font-lock-comment-face)

   :language 'greger
   :feature 'heading
   :override t
   '(;; Heading markers (##)
     (user "##" @font-lock-keyword-face)
     (assistant "##" @font-lock-keyword-face)
     (system "##" @font-lock-keyword-face)
     (thinking "##" @font-lock-keyword-face)
     (tool_use "##" @font-lock-keyword-face)
     (tool_result "##" @font-lock-keyword-face)
     (server_tool_use "##" @font-lock-keyword-face)
     (web_search_tool_result "##" @font-lock-keyword-face)
     (citations "##" @font-lock-keyword-face)

     ;; Heading types
     (user "USER" @font-lock-function-name-face)
     (assistant "ASSISTANT" @font-lock-function-name-face)
     (system "SYSTEM" @font-lock-function-name-face)
     (thinking "THINKING" @font-lock-function-name-face)
     (tool_use ["TOOL" "USE"] @font-lock-function-name-face)
     (tool_result ["TOOL" "RESULT"] @font-lock-function-name-face)
     (server_tool_use ["SERVER" "TOOL" "USE"] @font-lock-function-name-face)
     (web_search_tool_result ["WEB" "SEARCH" "TOOL" "RESULT"] @font-lock-function-name-face)
     (citations "CITATIONS" @font-lock-function-name-face)

     ;; Colons
     [":" @font-lock-builtin-face])

   :language 'greger
   :feature 'subheading
   :override t
   '(;; Sub-headings (###)
     (tool_param "###" @font-lock-keyword-face)
     (citation_entry "###" @font-lock-keyword-face))

   :language 'greger
   :feature 'field
   :override t
   '(;; Field names - these appear as string literals in the grammar
     ["Name:" "ID:" "Title:" "Cited text:" "Encrypted index:"] @font-lock-builtin-face)

   :language 'greger
   :feature 'value
   :override t
   '(;; Values
     (value) @font-lock-string-face
     (citation_url) @font-lock-constant-face)

   :language 'greger
   :feature 'code
   :override t
   '(;; Code blocks and inline code
     (code_block) @font-lock-string-face
     (inline_code) @font-lock-string-face)

   :language 'greger
   :feature 'markup
   :override t
   '(;; HTML-like tags
     (cite_tag) @font-lock-builtin-face
     (safe_shell_commands) @font-lock-builtin-face
     (tool_start_tag) @font-lock-builtin-face
     (tool_end_tag) @font-lock-builtin-face)

   :language 'greger
   :feature 'content
   :override t
   '(;; Content
     (tool_content) @font-lock-doc-face
     (text) @default)

   :language 'greger
   :feature 'error
   :override t
   '(;; Parse errors - highlighted with red background
     (ERROR) @grgfoo-error-face))
  "Tree-sitter font-lock settings for `grgfoo-mode'.")

(defface grgfoo-error-face
  '((t (:background "red" :foreground "white")))
  "Face for parse errors in grgfoo-mode."
  :group 'grgfoo)

(defvar grgfoo--treesit-indent-rules
  `((greger
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "source_file") parent-bol 0)
     ((parent-is "tool_use") parent-bol ,grgfoo-ts-indent-offset)
     ((parent-is "tool_result") parent-bol ,grgfoo-ts-indent-offset)
     ((parent-is "server_tool_use") parent-bol ,grgfoo-ts-indent-offset)
     ((parent-is "web_search_tool_result") parent-bol ,grgfoo-ts-indent-offset)
     ((parent-is "citations") parent-bol ,grgfoo-ts-indent-offset)
     (catch-all parent-bol 0)))
  "Tree-sitter indentation rules for `grgfoo-mode'.")

;;;###autoload
(define-derived-mode grgfoo-mode prog-mode "Greger"
  "Major mode for editing Greger files with tree-sitter support.

\\{grgfoo-mode-map}"
  :syntax-table grgfoo-mode-syntax-table

  (when (treesit-ready-p 'greger)
    ;; Tree-sitter setup
    (setq-local treesit-font-lock-settings grgfoo--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((error)           ; level 1 - always show errors
                  (comment heading) ; level 2 - basic structure
                  (field value subheading) ; level 3 - detailed structure
                  (code markup content))) ; level 4 - content highlighting

    ;; Indentation
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

;; Fallback when tree-sitter is not available
(unless (treesit-ready-p 'greger)
  (message "Tree-sitter not ready for 'greger language. Using basic mode."))

(defun grgfoo--defun-name (node)
  "Return the name of the defun NODE."
  (when node
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
      (_ nil))))

;; Ensure the grammar is loaded
(add-to-list 'treesit-extra-load-path default-directory)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grgfoo\\'" . grgfoo-mode))

(provide 'grgfoo)

;;; grgfoo.el ends here
