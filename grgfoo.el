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
(defun grgfoo--citation-entry-folding-function (node override start end)
  "Font-lock function to hide citation entries within assistant blocks.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (condition-case err
      (when grgfoo-citation-folding-enabled
        (when node
          (let* ((node-start (treesit-node-start node))
                 (node-end (treesit-node-end node))
                 (should-fold (not (get-text-property node-start 'grgfoo-citation-expanded))))
            (when should-fold
              ;; Hide the entire citation entry
              (put-text-property node-start (1- node-end) 'invisible 'grgfoo-citation)))))
    (error
     (message "ERROR in citation-entry-folding: %s" err))))

(defvar grgfoo--assistant-text-cache nil
  "Cache for assistant text parts to enable merging across multiple assistant blocks.")

(defun grgfoo--assistant-text-merger (node override start end)
  "Font-lock function to collect text from assistant blocks for merging.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (condition-case err
      (when grgfoo-citation-folding-enabled
        (when node
          ;; Extract text content from this assistant block
          (let* ((node-start (treesit-node-start node))
                 (node-end (treesit-node-end node))
                 (content-start (save-excursion
                                  (goto-char node-start)
                                  (forward-line 2) ; Skip "## ASSISTANT:" and blank line
                                  (point)))
                 (text-content (grgfoo--extract-text-from-assistant-block content-start node-end)))

            ;; Store this text for later merging
            (unless (string-empty-p text-content)
              (push (list node-start node-end content-start text-content) grgfoo--assistant-text-cache))

            ;; Check if this is the last assistant block by looking ahead
            (save-excursion
              (goto-char node-end)
              (let ((next-assistant (re-search-forward "^## ASSISTANT:$" nil t)))
                (unless next-assistant
                  ;; This is the last assistant block - time to merge
                  (grgfoo--apply-assistant-text-merging)))))))
    (error
     (message "ERROR in assistant-text-merger: %s" err))))

(defun grgfoo--apply-assistant-text-merging ()
  "Apply text merging using cached assistant text parts."
  (when (and grgfoo--assistant-text-cache
             (> (length grgfoo--assistant-text-cache) 1))
    (let* ((sorted-cache (sort grgfoo--assistant-text-cache
                              (lambda (a b) (< (car a) (car b)))))
           (first-entry (car sorted-cache))
           (first-content-start (nth 2 first-entry))
           (last-entry (car (last sorted-cache)))
           (last-node-end (nth 1 last-entry))
           (merged-text (string-join (mapcar (lambda (entry) (nth 3 entry)) sorted-cache) " ")))

      ;; Replace the entire range from first content to last assistant end
      (put-text-property first-content-start last-node-end 'display
                        (concat merged-text "\n\n"))))

  ;; Clear the cache
  (setq grgfoo--assistant-text-cache nil))

(defun grgfoo--apply-citation-folding ()
  "Apply comprehensive citation folding to merge assistant text blocks."
  (when grgfoo-citation-folding-enabled
    (save-excursion
      (let ((inhibit-read-only t)
            (modified (buffer-modified-p)))
        (condition-case err
            (progn
              ;; Clear any existing display properties first to avoid duplication
              (remove-text-properties (point-min) (point-max) '(display after-string))

              ;; Find all assistant blocks and collect their text content
              (goto-char (point-min))
              (let ((assistant-texts '())
                    (first-assistant-start nil)
                    (last-assistant-end nil)
                    (assistant-blocks '()))

                ;; Collect all assistant blocks and their text
                (while (re-search-forward "^## ASSISTANT:$" nil t)
                  (let* ((block-start (line-beginning-position))
                         (block-end (save-excursion
                                      (if (re-search-forward "^## " nil t)
                                          (line-beginning-position)
                                        (point-max))))
                         (content-start (save-excursion
                                          (goto-char block-start)
                                          (forward-line 2) ; Skip "## ASSISTANT:" and blank line
                                          (point)))
                         (text-content (grgfoo--extract-text-from-assistant-block content-start block-end)))

                    (when (not (string-empty-p text-content))
                      (push text-content assistant-texts))

                    (push (list block-start block-end content-start) assistant-blocks)
                    (unless first-assistant-start
                      (setq first-assistant-start block-start))
                    (setq last-assistant-end block-end)))

                ;; If we have multiple assistant blocks with text, merge them
                (when (> (length assistant-texts) 1)
                  (let ((merged-text (string-join (reverse assistant-texts) " ")))
                    ;; Find the range from first assistant content to last assistant end
                    (let* ((first-block (car (reverse assistant-blocks)))
                           (last-block (car assistant-blocks))
                           (first-content-start (nth 2 first-block))
                           (last-block-end (nth 1 last-block)))
                      ;; Replace entire range with merged content
                      (put-text-property first-content-start last-block-end 'display
                                       (concat merged-text "\n\n"))))))

              ;; Handle citations section
              (goto-char (point-min))
              (when (re-search-forward "^## CITATIONS:$" nil t)
                (let ((citations-start (line-beginning-position)))
                  (forward-line 1)
                  (let ((citations-content-start (point))
                        (citations-end (point-max)))
                    (when (< citations-content-start citations-end)
                      (put-text-property citations-content-start citations-end 'invisible 'grgfoo-citations)
                      ;; Count citations for summary
                      (let ((citation-count 0))
                        (goto-char citations-content-start)
                        (while (re-search-forward "^### " citations-end t)
                          (setq citation-count (1+ citation-count)))
                        (goto-char citations-start)
                        (end-of-line)
                        (put-text-property (point) (1+ (point)) 'after-string
                                         (propertize (format "[+%d citation%s, TAB to expand]"
                                                           citation-count
                                                           (if (= citation-count 1) "" "s"))
                                                   'face 'font-lock-comment-face))))))))
          (error
           (message "ERROR in apply-citation-folding: %s" err)))
        (set-buffer-modified-p modified)))))

(defun grgfoo--extract-text-from-assistant-block (start end)
  "Extract non-citation text from assistant block between START and END."
  (save-excursion
    (let ((text-parts '()))
      (goto-char start)
      (while (< (point) end)
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (when (< line-start line-end)
            (let ((line-text (buffer-substring-no-properties line-start line-end)))
              (unless (or (string-match-p "^###" line-text)
                         (string-match-p "^Title:" line-text)
                         (string-match-p "^Cited text:" line-text)
                         (string-match-p "^Encrypted index:" line-text)
                         (string-match-p "^## " line-text)
                         (string-empty-p (string-trim line-text)))
                (push (string-trim line-text) text-parts))))
          (forward-line 1)))
      (string-join (reverse text-parts) " "))))

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
     ;; Assistant text merging - collect and merge text across blocks
     (assistant) @grgfoo--assistant-text-merger
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

    ;; Initialize assistant text cache
    (when grgfoo-citation-folding-enabled
      (setq-local grgfoo--assistant-text-cache nil))

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
