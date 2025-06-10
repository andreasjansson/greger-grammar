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

(defgroup grgfoo nil
  "Major mode for Greger files."
  :group 'languages
  :prefix "grgfoo-")

(defcustom grgfoo-ts-indent-offset 2
  "Number of spaces for each indentation step in `grgfoo-mode'."
  :type 'integer
  :safe 'integerp
  :group 'grgfoo)

;; Customizable face colors for headers using standard Emacs color names
(defface grgfoo-user-header-face
  '((t (:foreground "cyan" :weight bold)))
  "Face for USER headers."
  :group 'grgfoo)

(defface grgfoo-assistant-header-face
  '((t (:foreground "green" :weight bold)))
  "Face for ASSISTANT headers."
  :group 'grgfoo)

(defface grgfoo-system-header-face
  '((t (:foreground "orange" :weight bold)))
  "Face for SYSTEM headers."
  :group 'grgfoo)

(defface grgfoo-thinking-header-face
  '((t (:foreground "magenta" :weight bold)))
  "Face for THINKING headers."
  :group 'grgfoo)

(defface grgfoo-tool-header-face
  '((t (:foreground "yellow" :weight bold)))
  "Face for tool-related headers (TOOL USE, TOOL RESULT, etc.)."
  :group 'grgfoo)

(defface grgfoo-citations-header-face
  '((t (:foreground "lightblue" :weight bold)))
  "Face for CITATIONS header."
  :group 'grgfoo)

(defface grgfoo-subheading-face
  '((t (:foreground "coral" :weight semi-bold)))
  "Face for subheadings like tool parameters and citation entries."
  :group 'grgfoo)

(defface grgfoo-field-name-face
  '((t (:foreground "lightyellow")))
  "Face for field names like 'Name:', 'ID:', etc."
  :group 'grgfoo)

(defface grgfoo-error-face
  '((t (:background "red" :foreground "white")))
  "Face for parse errors in grgfoo-mode."
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
   :feature 'headers
   :override t
   '(;; Major section headers - use custom faces for each type
     (user_header) @grgfoo-user-header-face
     (assistant_header) @grgfoo-assistant-header-face
     (system_header) @grgfoo-system-header-face
     (thinking_header) @grgfoo-thinking-header-face
     (tool_use_header) @grgfoo-tool-header-face
     (tool_result_header) @grgfoo-tool-header-face
     (server_tool_use_header) @grgfoo-tool-header-face
     (web_search_tool_result_header) @grgfoo-tool-header-face
     (citations_header) @grgfoo-citations-header-face)

   :language 'greger
   :feature 'subheadings
   :override t
   '(;; Sub-sections and parameter headers
     (tool_param) @grgfoo-subheading-face
     (citation_entry) @grgfoo-subheading-face)

   :language 'greger
   :feature 'fields
   :override t
   '(;; Field names only - no highlighting for values or content
     (name "Name:") @grgfoo-field-name-face
     (id "ID:") @grgfoo-field-name-face
     (citation_title "Title:") @grgfoo-field-name-face
     (citation_text "Cited text:") @grgfoo-field-name-face
     (citation_encrypted_index "Encrypted index:") @grgfoo-field-name-face)

   :language 'greger
   :feature 'comments
   :override t
   '(;; HTML comments only
     (html_comment) @font-lock-comment-face)

   :language 'greger
   :feature 'error
   :override t
   '(;; Parse errors
     (ERROR) @grgfoo-error-face))
  "Tree-sitter font-lock settings for `grgfoo-mode'.")

(defvar grgfoo--treesit-indent-rules
  `((greger
     ;; Working indentation rules that avoid parent-is (which causes segfaults)
     ((node-is "user") column-0 0)
     ((node-is "assistant") column-0 0)
     ((node-is "system") column-0 0)
     ((node-is "thinking") column-0 0)
     ((node-is "tool_use") column-0 0)
     ((node-is "tool_result") column-0 0)
     ((node-is "server_tool_use") column-0 0)
     ((node-is "web_search_tool_result") column-0 0)
     ((node-is "citations") column-0 0)
     ;; Indent content within sections
     ((node-is "text") parent-bol ,grgfoo-ts-indent-offset)
     ((node-is "name") parent-bol ,grgfoo-ts-indent-offset)
     ((node-is "id") parent-bol ,grgfoo-ts-indent-offset)
     ((node-is "value") parent-bol ,grgfoo-ts-indent-offset)
     ;; Default handling
     (no-node column-0 0)
     (catch-all column-0 0)))
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
                  (headers)                  ; level 2 - main headers
                  (subheadings fields)       ; level 3 - subheadings and field names
                  (comments)))               ; level 4 - comments only, no text content

    ;; Indentation - using simple and safe rules
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
