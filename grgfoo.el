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


(defun grgfoo--node-end-no-whitespace (node)
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (text (buffer-substring-no-properties start end))
         (last-non-whitespace (1- (when (string-match "\\S-\\s-*\\'" text)
                                 (match-beginning 0)))))

    ;; TODO: remove debug
    (message (format "text: %s" text))

    ;; TODO: remove debug
    (message (format "(- end start): %s" (- end start)))
    ;; TODO: remove debug
    (message (format "last-non-whitespace: %s" last-non-whitespace))

    (if last-non-whitespace
        (+ start last-non-whitespace)
      end)))

(defun grgfoo--node-start-no-whitespace (node)
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (text (buffer-substring-no-properties start end))
         (first-non-whitespace (string-match "[^ \t\n\r\f]" text)))
    (if first-non-whitespace
        (+ start first-non-whitespace)
      start)))

;; Citation folding functions
(defun grgfoo--citation-entry-folding-function (node override start end)
  "Font-lock function to hide citation entries within assistant blocks.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (when node
    (let* ((node-start (treesit-node-start node))
           (node-end (grgfoo--node-end-no-whitespace node))
           (parent (treesit-node-parent node))
           (text (treesit-search-subtree parent "^text$" t nil 1))
           (text-start (treesit-node-start text))
           (text-end (grgfoo--node-end-no-whitespace text))
           (uncle (treesit-node-prev-sibling parent))
           (aunt (treesit-node-next-sibling parent))
           (should-fold (not (get-text-property text-start 'grgfoo-citation-expanded)))
           (invisible-start text-end)
           (invisible-end node-end))

      (when (and aunt (equal (treesit-node-type aunt) "assistant"))
        (let* ((aunt-first-child (treesit-node-child aunt 1)) ;; skip header
               (aunt-first-child-start (grgfoo--node-start-no-whitespace aunt-first-child)))
          ;; space for displayed " "
          (setq invisible-end (1- aunt-first-child-start))

          (if should-fold
              (put-text-property invisible-end (1+ invisible-end) 'display " ")
            (remove-text-properties invisible-end (1+ invisible-end) '(display nil)))))

      (message (format "text-start: %s" text-start))
      (message (format "text-end: %s" text-end))
      (put-text-property text-start text-end 'face 'underline)
      (put-text-property text-start text-end 'mouse-face 'highlight)
      (put-text-property text-start text-end 'grgfoo-expandable-citation-entry t)
      (put-text-property text-start text-end 'invisible-start invisible-start)
      (put-text-property text-start text-end 'invisible-end invisible-end)

      (put-text-property invisible-start invisible-end 'invisible should-fold)

      (when (and uncle (equal (treesit-node-type uncle) "assistant"))
        (let* ((uncle-last-citation-entry (treesit-search-subtree uncle "^citation_entry$" t nil 1)))

          (if uncle-last-citation-entry
              ;; uncle always invisible
              (put-text-property (treesit-node-end uncle-last-citation-entry) (1- node-start) 'invisible t))

          (let* ((uncle-last-child (treesit-node-child uncle -1))
                 (uncle-last-child-end (grgfoo--node-end-no-whitespace uncle-last-child)))

            (put-text-property (- text-start 1) text-start 'display " ")

            ;; uncle always invisible
            (put-text-property (1- uncle-last-child-end) (1- text-start) 'invisible t))))

      )))

(defun grgfoo--citations-section-folding-function (node override start end)
  "Font-lock function to handle citations section folding.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (should-fold (not (get-text-property node-start 'grgfoo-citations-expanded))))

    (let* ((text (buffer-substring-no-properties node-start node-end))
           (first-newline (string-search "\n" text))
           (header-end (if first-newline
                           (+ node-start first-newline)
                         node-end)))
      ;; Make everything after the header invisible
      (when (< header-end node-end)
        (put-text-property (1+ header-end) node-end 'invisible should-fold)
        ;; Add summary text with citation count
        (let ((citation-count (grgfoo--count-citations-in-section node)))
          (put-text-property header-end (1+ header-end) 'after-string
                             (propertize (format "\n[+%d citation%s, TAB to expand]"
                                                 citation-count
                                                 (if (= citation-count 1) "" "s"))
                                         'face 'font-lock-comment-face)))))))

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
   '(;; Citation folding - hide individual citations within assistant blocks
     (assistant (citation_entry) @grgfoo--citation-entry-folding-function)
     ;; Assistant text merging - merge text across multiple assistant blocks
     ;(assistant) @grgfoo--assistant-merger-function
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

    ;; Setup key bindings
    (local-set-key (kbd "TAB") #'grgfoo-toggle-fold)

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

(defun grgfoo--count-citations-in-section (citations-node)
  "Count the number of citation entries in CITATIONS-NODE."
  (condition-case nil
    (length (treesit-query-capture citations-node '((citation_entry) @citation)))
    (error 0)))

(defun grgfoo-toggle-fold ()
  "Toggle folding of citation at point."
  (interactive)

  (if (get-text-property (point) 'grgfoo-expandable-citation-entry)
      (let* ((node (treesit-node-at (point)))
             (node-start (treesit-node-start node))
             (invisible-start (get-text-property node-start 'grgfoo-invisible-start))
             (invisible-end (get-text-property node-start 'grgfoo-invisible-end))
             (is-expanded (get-text-property node-start 'grgfoo-citation-expanded)))
        (put-text-property node-start (1+ node-start) 'grgfoo-citation-expanded (not is-expanded))
        (font-lock-flush invisible-start invisible-end))
    (indent-for-tab-command)))


;; Ensure the grammar is loaded
(add-to-list 'treesit-extra-load-path default-directory)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grgfoo\\'" . grgfoo-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.greger\\'" . grgfoo-mode))

(provide 'grgfoo)

;;; grgfoo.el ends here
