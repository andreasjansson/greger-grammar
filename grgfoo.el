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
(require 'cl-lib)

(defgroup grgfoo nil
  "Major mode for Greger files."
  :group 'languages
  :prefix "grgfoo-")

(defcustom grgfoo-ts-indent-offset 2
  "Number of spaces for each indentation step in `grgfoo-mode'."
  :type 'integer
  :safe 'integerp
  :group 'grgfoo)

(defcustom grgfoo-citation-folding-enabled t
  "Whether to enable citation folding by default."
  :type 'boolean
  :group 'grgfoo)

(defcustom grgfoo-citation-summary-face 'underline
  "Face to use for citation text when folded."
  :type 'face
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

(defface grgfoo-tool-tag-face
  '((t (:foreground "gray" :height 0.8)))
  "Face for tool start and end tags."
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

;; Citation folding functions
(defun grgfoo--citation-folding-function (node override start end)
  "Font-lock function to handle citation folding.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (condition-case err
      (when grgfoo-citation-folding-enabled
        (message "DEBUG: citation-folding-function called, node=%s" (if node "present" "nil"))
        (when node
          (let* ((node-start (treesit-node-start node))
                 (node-end (treesit-node-end node))
                 (should-fold (not (get-text-property node-start 'grgfoo-citation-expanded))))
            (message "DEBUG: node type=%s start=%d end=%d should-fold=%s"
                     (treesit-node-type node) node-start node-end should-fold)
            (when should-fold
              ;; Hide the entire citation block
              (put-text-property node-start (1- node-end) 'invisible 'grgfoo-citation)
              (message "DEBUG: Applied invisible property from %d to %d" node-start (1- node-end))))))
    (error
     (message "ERROR in citation-folding-function: %s" err))))

(defun grgfoo--citations-section-folding-function (node override start end)
  "Font-lock function to handle citations section folding.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (when grgfoo-citation-folding-enabled
    (let* ((node-start (treesit-node-start node))
           (node-end (treesit-node-end node))
           (should-fold (not (get-text-property node-start 'grgfoo-citations-expanded))))
      (when should-fold
        ;; Find the header line by looking for the first newline
        (let* ((text (buffer-substring-no-properties node-start node-end))
               (first-newline (string-search "\n" text))
               (header-end (if first-newline
                             (+ node-start first-newline)
                             node-end)))
          ;; Make everything after the header invisible
          (when (< header-end node-end)
            (put-text-property (1+ header-end) node-end 'invisible 'grgfoo-citations)
            ;; Add summary text with citation count
            (let ((citation-count (grgfoo--count-citations-in-section node)))
              (put-text-property header-end (1+ header-end) 'after-string
                               (propertize (format "\n[+%d citation%s, TAB to expand]"
                                                 citation-count
                                                 (if (= citation-count 1) "" "s"))
                                         'face 'font-lock-comment-face)))))))))

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
   :feature 'folding
   :override t
   '(;; Citation folding - hide individual citations
     (assistant (citation_entry) @grgfoo--citation-folding-function)
     ;; Citations section folding
     (citations) @grgfoo--citations-section-folding-function)

   :language 'greger
   :feature 'subheadings
   :override t
   '(;; Sub-sections and parameter headers
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
   :feature 'tool-tags
   :override t
   '(;; Tool start and end tags - smaller and less visible
     (tool_start_tag) @grgfoo-tool-tag-face
     (tool_end_tag) @grgfoo-tool-tag-face)

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
                '((error)
                  (headers folding)
                  (tool-tags comments)
                  (subheadings fields)
                  (tool-tags comments)))

    ;; Indentation - using simple and safe rules
    (setq-local treesit-simple-indent-rules grgfoo--treesit-indent-rules)

    ;; Navigation - treat headings as defuns for C-M-a and C-M-e
    (setq-local treesit-defun-type-regexp
                (rx (or "user" "assistant" "system" "thinking"
                        "tool_use" "tool_result" "server_tool_use"
                        "web_search_tool_result" "citations")))

    ;; Set up defun name function to show heading type
    (setq-local treesit-defun-name-function #'grgfoo--defun-name)

    ;; Setup citation folding invisibility
    (when grgfoo-citation-folding-enabled
      (add-to-invisibility-spec 'grgfoo-citation)
      (add-to-invisibility-spec 'grgfoo-citations))

    ;; Setup key bindings
    (local-set-key (kbd "TAB") #'grgfoo-toggle-citation-fold)

    ;; Enable all tree-sitter features
    (treesit-major-mode-setup)

    ))



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

(defun grgfoo--find-citation-at-point ()
  "Find citation node at point, if any - using safer approach."
  (condition-case err
    (progn

      (if (treesit-ready-p 'greger)
          (let ((node (treesit-node-at (point))))
              (if node
                  (progn
                    ;; Check if we're already on a citation_entry or citations node
                    (if (member (treesit-node-type node) '("citation_entry" "citations"))
                        node
                      ;; Check immediate parent only to avoid segfaults
                      (let ((parent (treesit-node-parent node)))
                        (if (and parent (member (treesit-node-type parent) '("citation_entry" "citations")))
                            parent
                          nil))))
                nil))
        nil))
    (error
     (message "ERROR in find-citation: %s" err)
     nil)))

(defun grgfoo--count-citations-in-section (citations-node)
  "Count the number of citation entries in CITATIONS-NODE."
  (condition-case nil
    (length (treesit-query-capture citations-node '((citation_entry) @citation)))
    (error 0)))

(defun grgfoo-toggle-citation-fold ()
  "Toggle folding of citation at point."
  (interactive)
  (condition-case err
      (progn

        (if grgfoo-citation-folding-enabled
            (progn

              (let ((citation-node (grgfoo--find-citation-at-point)))
                (if citation-node
                    (progn
                      (message "DEBUG TAB: found citation node type=%s start=%s end=%s"
                               (treesit-node-type citation-node)
                               (treesit-node-start citation-node)
                               (treesit-node-end citation-node))
                      (let* ((node-start (treesit-node-start citation-node))
                             (node-end (treesit-node-end citation-node))
                             (node-type (treesit-node-type citation-node))
                             (is-citations-section (string= node-type "citations")))
                        (if is-citations-section
                            ;; Handle citations section
                            (let ((is-expanded (get-text-property node-start 'grgfoo-citations-expanded)))
                              (if is-expanded
                                  ;; Collapse citations section
                                  (progn
                                    (remove-text-properties node-start (1+ node-start) '(grgfoo-citations-expanded))
                                    (message "Citations section collapsed"))
                                ;; Expand citations section
                                (progn
                                  (put-text-property node-start (1+ node-start) 'grgfoo-citations-expanded t)
                                  ;; Clear existing invisible properties
                                  (remove-text-properties node-start node-end '(invisible after-string))
                                  (message "Citations section expanded"))))
                          ;; Handle individual citation
                          (let ((is-expanded (get-text-property node-start 'grgfoo-citation-expanded)))
                            (if is-expanded
                                ;; Collapse citation
                                (progn
                                  (remove-text-properties node-start (1+ node-start) '(grgfoo-citation-expanded))
                                  ;; Clear all text properties to ensure fresh font-lock
                                  (remove-text-properties node-start node-end '(invisible face))
                                  (message "Citation collapsed"))
                              ;; Expand citation
                              (progn
                                (put-text-property node-start (1+ node-start) 'grgfoo-citation-expanded t)
                                ;; Clear existing invisible properties
                                (remove-text-properties node-start node-end '(invisible face))
                                (message "Citation expanded")))))
                        ;; Trigger font-lock refresh
                        (font-lock-flush node-start node-end)))
                  (message "DEBUG TAB: no citation node found, falling back"))
                (indent-for-tab-command)))
          (progn
            (message "DEBUG TAB: citation folding disabled, falling back")
            (indent-for-tab-command))))
    (error
     (message "Error in citation folding: %s" err)
     (indent-for-tab-command))))

;; Ensure the grammar is loaded
(add-to-list 'treesit-extra-load-path default-directory)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grgfoo\\'" . grgfoo-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.greger\\'" . grgfoo-mode))

(provide 'grgfoo)

;;; grgfoo.el ends here
