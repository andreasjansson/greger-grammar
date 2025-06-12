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
  '((t (:foreground "cyan" :weight bold :height 1.1)))
  "Face for USER headers."
  :group 'grgfoo)

(defface grgfoo-assistant-header-face
  '((t (:foreground "green" :weight bold :height 1.1)))
  "Face for ASSISTANT headers."
  :group 'grgfoo)

(defface grgfoo-system-header-face
  '((t (:foreground "orange" :weight bold :height 1.1)))
  "Face for SYSTEM headers."
  :group 'grgfoo)

(defface grgfoo-thinking-header-face
  '((t (:foreground "magenta" :weight bold :height 1.1)))
  "Face for THINKING headers."
  :group 'grgfoo)

(defface grgfoo-tool-header-face
  '((t (:foreground "yellow" :weight bold :height 1.1)))
  "Face for tool-related headers (TOOL USE, TOOL RESULT, etc.)."
  :group 'grgfoo)

(defface grgfoo-subheading-face
  '((t (:foreground "coral" :weight semi-bold)))
  "Face for subheadings like tool parameters and citation entries."
  :group 'grgfoo)

(defface grgfoo-field-name-face
  '((t (:foreground "lightyellow")))
  "Face for field names like 'Name:', 'ID:', etc."
  :group 'grgfoo)

(defface grgfoo-tool-param-name-face
  '((t (:foreground "lightgreen")))
  "Face for tool parameter names like 'path', 'content', etc."
  :group 'grgfoo)

(defface grgfoo-key-face
  '((t (:foreground "lightblue")))
  "Face for tool parameter names like 'path', 'content', etc."
  :group 'grgfoo)

(defface grgfoo-tool-tag-face
  '((t (:foreground "gray" :height 0.6)))
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

;; Tool folding functions
(defun grgfoo--tool-content-tail-folding-function (node override start end)
  "Font-lock function to make tool_content_tail invisible by default.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (when (and node (treesit-node-p node))
    (condition-case err
        (let* ((node-start (treesit-node-start node))
               (node-end (treesit-node-end node))
               (is-visible (get-text-property node-start 'grgfoo-tool-content-expanded)))
          
          ;; Apply invisibility (default is invisible unless expanded)
          (put-text-property node-start node-end 'invisible (not is-visible)))
      (error (message "Error in tool-content-tail folding: %s" err)))))

(defun grgfoo--tool-content-head-folding-function (node override start end)
  "Font-lock function to make tool_content_head TAB-able for controlling tail visibility.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (when (and node (treesit-node-p node))
    (condition-case err
        (let* ((node-start (treesit-node-start node))
               (node-end (treesit-node-end node))
               (parent (treesit-node-parent node)))
          
          (when (and parent (treesit-node-p parent))
            ;; Find the corresponding tail safely
            (let ((tail-node (treesit-search-subtree parent "^tool_content_tail$" nil nil 1)))
              
              (when (and tail-node (treesit-node-p tail-node))
                (let* ((tail-start (treesit-node-start tail-node))
                       (tail-end (treesit-node-end tail-node))
                       (is-tail-visible (get-text-property tail-start 'grgfoo-tool-content-expanded))
                       (line-count (max 1 (count-lines tail-start tail-end))))
                  
                  ;; Mark the head as foldable and store tail info
                  (put-text-property node-start node-end 'grgfoo-foldable-tool-content t)
                  (put-text-property node-start node-end 'grgfoo-tool-tail-start tail-start)
                  (put-text-property node-start node-end 'grgfoo-tool-tail-end tail-end)
                  
                  ;; Clean up old overlays first
                  (let ((old-overlay (get-text-property node-start 'grgfoo-fold-overlay)))
                    (when (and old-overlay (overlayp old-overlay))
                      (delete-overlay old-overlay)
                      (remove-text-properties node-start node-end '(grgfoo-fold-overlay nil))))
                  
                  ;; Add overlay with fold indicator when tail is hidden
                  (unless is-tail-visible
                    (let ((overlay (make-overlay (-  node-end 2) (1- node-end))))
                      (overlay-put overlay 'after-string 
                                   (propertize (format "\n[+%d lines, TAB to expand]" line-count)
                                               'face '(:foreground "gray" :height 0.8 :slant italic)))
                      (overlay-put overlay 'grgfoo-fold-overlay t)
                      (overlay-put overlay 'evaporate t)
                      ;; Store overlay reference for cleanup
                      (put-text-property node-start node-end 'grgfoo-fold-overlay overlay))))))))
      (error (message "Error in tool-content-head folding: %s" err)))))

;; Citation keymap for mouse clicks
(defvar grgfoo-citation-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'grgfoo-toggle-fold)
    map)
  "Keymap for citation text to handle mouse clicks.")

;; Citation folding functions
(defun grgfoo--citation-entry-folding-function (node override start end)
  "Font-lock function to hide citation entries within assistant blocks.
NODE is the matched tree-sitter node, OVERRIDE is the override setting,
START and END are the region bounds."
  (when node
    (let* ((node-start (treesit-node-start node))
           (node-end (grgfoo--node-end-no-whitespace node))
           (parent (treesit-node-parent node))
           (first-text (treesit-search-subtree parent "^text$" nil nil 1))
           (last-text (treesit-search-subtree parent "^text$" t nil 1))
           (text-start (treesit-node-start first-text))
           (text-end (grgfoo--node-end-no-whitespace last-text))
           (uncle (treesit-node-prev-sibling parent))
           (aunt (treesit-node-next-sibling parent))
           (should-fold (not (get-text-property text-start 'grgfoo-citation-expanded)))
           (invisible-start text-end)
           (invisible-end node-end))

      (put-text-property text-start text-end 'face '(:underline "#555588"))
      (put-text-property text-start text-end 'mouse-face 'highlight)
      (put-text-property text-start text-end 'grgfoo-expandable-citation-entry t)
      (put-text-property text-start text-end 'keymap grgfoo-citation-keymap)
      (put-text-property text-start text-end 'invisible-start invisible-start)
      (put-text-property text-start text-end 'invisible-end invisible-end)

      (when (and uncle (equal (treesit-node-type uncle) "assistant"))
        (let* ((uncle-last-citation-entry (treesit-search-subtree uncle "^citation_entry$" t nil 1)))

          (if uncle-last-citation-entry
              ;; uncle always invisible
              (put-text-property (treesit-node-end uncle-last-citation-entry) node-start 'invisible t)

            (let* ((uncle-last-child (treesit-node-child uncle -1))
                   (uncle-last-child-end (grgfoo--node-end-no-whitespace uncle-last-child)))

              ;; uncle always invisible
              (put-text-property uncle-last-child-end text-start 'invisible t)))

          ))

      (when (and aunt (equal (treesit-node-type aunt) "assistant"))
        (let* ((aunt-first-child (treesit-node-child aunt 1)) ;; skip header
               (aunt-first-child-start (grgfoo--node-start-no-whitespace aunt-first-child)))
          ;; space for displayed " "
          (setq invisible-end (+ aunt-first-child-start 2))

          ))

      (put-text-property invisible-start invisible-end 'invisible should-fold)

      

      

      )))

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
     (web_search_tool_result_header) @grgfoo-tool-header-face)

   :language 'greger
   :feature 'folding
   :override t
   '(;; Citation folding - hide individual citations within assistant blocks
     (assistant (citation_entry) @grgfoo--citation-entry-folding-function)

     ;; TOol folding
     (tool_content_tail) @grgfoo--tool-content-tail-folding-function
     (tool_content_head) @grgfoo--tool-content-head-folding-function)

   :language 'greger
   :feature 'subheadings
   :override t
   '(;; Sub-sections and parameter headers
     (citation_entry) @grgfoo-subheading-face)

   :language 'greger
   :feature 'fields
   :override t
   '(;; Field names only - no highlighting for values or content

     ;; DEBUG: Try to match ALL text to see if this rule works at all
     ;; Tool parameter names - match name nodes that aren't literal strings
     (tool_param_header) @grgfoo-tool-param-name-face
     (key) @grgfoo-key-face
     )

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
     ;; Indent content within sections
     ;; Default handling
     ;(no-node column-0 0)
     ;(catch-all column-0 0)
     ))
  "Tree-sitter indentation rules for `grgfoo-mode'.")

;;;###autoload
(define-derived-mode grgfoo-mode prog-mode "Greger"
  "Major mode for editing Greger files with tree-sitter support.

\\{grgfoo-mode-map}"
  :syntax-table grgfoo-mode-syntax-table

  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter for Greger isn't available"))

  (treesit-parser-create 'greger)

  ;; Tree-sitter setup
  (setq-local treesit-font-lock-settings grgfoo--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((error)
                (headers folding tool-folding fields)
                (tool-tags comments)
                (subheadings)))

  ;; Indentation - using simple and safe rules
  (setq-local treesit-simple-indent-rules grgfoo--treesit-indent-rules)

    (setq-local treesit-defun-prefer-top-level t)

    ;; Disabled because this crashes emacs.
    ;; Reproduce: At beginning of buffer, run (treesit-search-forward-goto (treesit-node-at (point)) "" t t t)
    ;; Navigation - treat headings as defuns for C-M-a and C-M-e
    ;; (setq-local treesit-defun-type-regexp
    ;;             (rx line-start (or "user" "assistant") line-end))
    ;; Set up defun name function to show heading type
    ;; (setq-local treesit-defun-name-function #'grgfoo--defun-name)

  ;; Setup key bindings
  (local-set-key (kbd "TAB") #'grgfoo-toggle-fold)

  ;; Enable all tree-sitter features
  (treesit-major-mode-setup)

  )

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
          (_ nil))
      (error nil))))

(defun grgfoo-toggle-fold ()
  "Toggle folding of citation or tool content at point."
  (interactive)

  (let ((node (treesit-node-at (point))))
    (cond
     ;; Handle citation folding
     ((get-text-property (point) 'grgfoo-expandable-citation-entry)
      (let* ((node-start (treesit-node-start node))
             (invisible-start (get-text-property node-start 'invisible-start))
             (invisible-end (get-text-property node-start 'invisible-end))
             (is-expanded (get-text-property node-start 'grgfoo-citation-expanded)))
        (put-text-property node-start (1+ node-start) 'grgfoo-citation-expanded (not is-expanded))
        (font-lock-flush invisible-start invisible-end)))
   
     ;; Handle tool content folding - TAB in tool_content_head
     ((get-text-property (point) 'grgfoo-foldable-tool-content)
      (let* ((tail-start (get-text-property (point) 'grgfoo-tool-tail-start))
             (tail-end (get-text-property (point) 'grgfoo-tool-tail-end)))
        (when (and tail-start tail-end)
          (let ((is-tail-visible (get-text-property tail-start 'grgfoo-tool-content-expanded)))
            (put-text-property tail-start (min (1+ tail-start) tail-end) 'grgfoo-tool-content-expanded (not is-tail-visible))
            ;; Also need to flush both head and tail for overlay updates
            (font-lock-flush (point) tail-end)))))

     ;; Check if we're inside tool_content_tail when it's visible
     ((and node (equal (treesit-node-type node) "tool_content_tail"))
      ;; Check if current node is tool_content_tail or inside it
      (let* ((tail-start (treesit-node-start node))
             (tail-end (treesit-node-end node))
             (is-tail-visible (get-text-property tail-start 'grgfoo-tool-content-expanded)))
        ;; Only toggle if tail is currently visible
        (when is-tail-visible
          (put-text-property tail-start (1+ tail-start) 'grgfoo-tool-content-expanded nil)
          ;; Find the corresponding head to flush it too for overlay updates
          (let* ((parent (treesit-node-parent node))
                 (head-node (treesit-search-subtree parent "^tool_content_head$" nil nil 1)))
            (when head-node
              (font-lock-flush (treesit-node-start head-node) tail-end)))
          t)))
   
     ;; Default behavior
     (t (indent-for-tab-command))))
  )


;; Ensure the grammar is loaded
(add-to-list 'treesit-extra-load-path default-directory)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.grgfoo\\'" . grgfoo-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.greger\\'" . grgfoo-mode))

(provide 'grgfoo)

;;; grgfoo.el ends here
