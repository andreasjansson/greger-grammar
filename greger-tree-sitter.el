(require 'treesit)

(add-to-list 'treesit-extra-load-path "/Users/andreas/scratch/greger-grammar")

(defun greger-tree-sitter-parse (text)
  "Parse greger conversation TEXT using tree-sitter and return structured dialog."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (greger-tree-sitter--extract-dialog-from-node root-node))))

(defun greger-tree-sitter--extract-dialog-from-node (root-node)
  "Extract dialog structure from parsed greger conversation."
  (let* ((sections (treesit-node-children root-node))
         (dialog '())
         (pending-assistant-content '()))

    (dolist (section sections)
      (let ((section-type (treesit-node-type section)))
        (cond
         ((string= section-type "user_section")
          ;; Flush any pending assistant content before processing user section
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              ;; Simplify content if it's just a single text block
              (if (and (= (length content) 1)
                       (equal (alist-get 'type (car content)) "text"))
                  (push `((role . "assistant")
                          (content . ,(alist-get 'text (car content))))
                        dialog)
                (push `((role . "assistant")
                        (content . ,content))
                      dialog)))
            (setq pending-assistant-content '()))
          (push (greger-tree-sitter--extract-user-section section) dialog))

         ((string= section-type "assistant_section")
          ;; Add assistant text to pending content (don't flush yet)
          (let ((assistant-text (greger-tree-sitter--extract-section-text section)))
            (when (> (length (string-trim assistant-text)) 0)
              (push `((type . "text")
                      (text . ,assistant-text))
                    pending-assistant-content))))

         ((string= section-type "system_section")
          ;; Flush any pending assistant content before processing system section
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              ;; Simplify content if it's just a single text block
              (if (and (= (length content) 1)
                       (equal (alist-get 'type (car content)) "text"))
                  (push `((role . "assistant")
                          (content . ,(alist-get 'text (car content))))
                        dialog)
                (push `((role . "assistant")
                        (content . ,content))
                      dialog)))
            (setq pending-assistant-content '()))
          (push (greger-tree-sitter--extract-system-section section) dialog))

         ((string= section-type "thinking_section")
          ;; Add thinking to pending assistant content
          (let ((thinking-content (greger-tree-sitter--extract-section-text section)))
            (push `((type . "thinking")
                    (thinking . ,thinking-content))
                  pending-assistant-content)))

         ((string= section-type "tool_use_section")
          ;; Add tool use to pending assistant content
          (let ((tool-use-data (greger-tree-sitter--extract-tool-use section)))
            (push tool-use-data pending-assistant-content)))

         ((string= section-type "tool_result_section")
          ;; Flush any pending assistant content and add tool result as user content
          (when pending-assistant-content
            (let ((content (nreverse pending-assistant-content)))
              ;; Simplify content if it's just a single text block
              (if (and (= (length content) 1)
                       (equal (alist-get 'type (car content)) "text"))
                  (push `((role . "assistant")
                          (content . ,(alist-get 'text (car content))))
                        dialog)
                (push `((role . "assistant")
                        (content . ,content))
                      dialog)))
            (setq pending-assistant-content '()))
          (let ((tool-result-data (greger-tree-sitter--extract-tool-result section)))
            (push `((role . "user")
                    (content . (,tool-result-data)))
                  dialog)))

         ((string= section-type "server_tool_use_section")
          ;; Add server tool use to pending assistant content
          (let ((server-tool-use-data (greger-tree-sitter--extract-server-tool-use section)))
            (push server-tool-use-data pending-assistant-content)))

         ((string= section-type "server_tool_result_section")
          ;; Add server tool result to pending assistant content
          (let ((server-tool-result-data (greger-tree-sitter--extract-server-tool-result section)))
            (push server-tool-result-data pending-assistant-content))))))

    ;; Flush any remaining pending assistant content
    (when pending-assistant-content
      (let ((content (nreverse pending-assistant-content)))
        ;; Simplify content if it's just a single text block
        (if (and (= (length content) 1)
                 (equal (alist-get 'type (car content)) "text"))
            (push `((role . "assistant")
                    (content . ,(alist-get 'text (car content))))
                  dialog)
          (push `((role . "assistant")
                  (content . ,content))
                dialog))))

    (nreverse dialog)))

(defun greger-tree-sitter--extract-user-section (section-node)
  "Extract user section content."
  (let ((content (greger-tree-sitter--extract-section-text section-node)))
    `((role . "user")
      (content . ,content))))

(defun greger-tree-sitter--extract-assistant-section (section-node)
  "Extract assistant section content."
  (let ((content (greger-tree-sitter--extract-section-text section-node)))
    `((role . "assistant")
      (content . ,content))))

(defun greger-tree-sitter--extract-system-section (section-node)
  "Extract system section content."
  (let ((content (greger-tree-sitter--extract-section-text section-node)))
    `((role . "system")
      (content . ,content))))

(defun greger-tree-sitter--extract-section-text (section-node)
  "Extract text content from a section node."
  (let ((children (treesit-node-children section-node)))
    (string-trim
     (mapconcat (lambda (child)
                  (let ((node-type (treesit-node-type child)))
                    (cond
                     ((string= node-type "text_block")
                      (treesit-node-text child))
                     ;; Could add other content types here like code_block, cite_tag, etc.
                     (t ""))))
                children ""))))

(defun greger-tree-sitter--extract-tool-use (tool-use-section)
  "Extract tool use data from a tool use section."
  (let ((children (treesit-node-children tool-use-section))
        (name nil)
        (id nil)
        (input '()))

    ;; Extract metadata and parameters
    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "text_block")
          ;; Parse Name: and ID: lines from text_block
          (let ((text (treesit-node-text child)))
            (dolist (line (split-string text "\n"))
              (setq line (string-trim line))
              (cond
               ((string-prefix-p "Name:" line)
                (setq name (string-trim (substring line 5))))
               ((string-prefix-p "ID:" line)
                (setq id (string-trim (substring line 3))))))))

         ((string= node-type "tool_param")
          ;; Extract parameter from tool_param node
          (let* ((param-text (treesit-node-text child))
                 (lines (split-string param-text "\n"))
                 (header-line (string-trim (car lines)))
                 (param-name (if (string-prefix-p "###" header-line)
                                 (string-trim (substring header-line 3))
                               header-line))
                 (param-children (treesit-node-children child)))
            ;; Look for tool_content in the children
            (dolist (param-child param-children)
              (when (string= (treesit-node-type param-child) "tool_content")
                (let ((content-text (string-trim (treesit-node-text param-child))))
                  ;; Remove the <tool.ID> wrapper - note that the closing > might be missing
                  (cond
                   ;; Full wrapper case
                   ((string-match "^<tool\\.[^>]+>\\s-*\\(.*?\\)\\s-*</tool\\.[^>]+>$" content-text)
                    (setq content-text (match-string 1 content-text)))
                   ;; Partial wrapper case (missing closing >)
                   ((string-match "^<tool\\.[^>]+>\\s-*\\(.*?\\)\\s-*</tool\\.[^>]+$" content-text)
                    (setq content-text (match-string 1 content-text)))
                   ;; Just remove the opening tag if present
                   ((string-match "^<tool\\.[^>]+>\\s-*\\(.*\\)" content-text)
                    (setq content-text (match-string 1 content-text))))
                  (setq content-text (string-trim content-text))
                  (push (cons (intern param-name) content-text) input)))))))))

    `((type . "tool_use")
      (id . ,id)
      (name . ,name)
      (input . ,(nreverse input)))))

(defun greger-tree-sitter--extract-tool-result (tool-result-section)
  "Extract tool result data from a tool result section."
  (let ((children (treesit-node-children tool-result-section))
        (tool-use-id nil)
        (content nil))

    ;; Extract ID and content
    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "text_block")
          ;; Parse ID: line from text_block
          (let ((text (treesit-node-text child)))
            (dolist (line (split-string text "\n"))
              (setq line (string-trim line))
              (when (string-prefix-p "ID:" line)
                (setq tool-use-id (string-trim (substring line 3)))))))

         ((string= node-type "tool_content")
          ;; Extract content from tool_content node
          (let ((content-text (string-trim (treesit-node-text child))))
            ;; Remove the <tool.ID> wrapper - note that the closing > might be missing
            (cond
             ;; Full wrapper case
             ((string-match "^<tool\\.[^>]+>\\s-*\\(.*?\\)\\s-*</tool\\.[^>]+>$" content-text)
              (setq content-text (match-string 1 content-text)))
             ;; Partial wrapper case (missing closing >)
             ((string-match "^<tool\\.[^>]+>\\s-*\\(.*?\\)\\s-*</tool\\.[^>]+$" content-text)
              (setq content-text (match-string 1 content-text)))
             ;; Just remove the opening tag if present
             ((string-match "^<tool\\.[^>]+>\\s-*\\(.*\\)" content-text)
              (setq content-text (match-string 1 content-text))))
            (setq content (string-trim content-text)))))))

    `((type . "tool_result")
      (tool_use_id . ,tool-use-id)
      (content . ,content))))

(defun greger-tree-sitter--extract-server-tool-use (server-tool-use-section)
  "Extract server tool use data from a server tool use section."
  ;; Similar to tool use but with server_tool_use type
  (let ((result (greger-tree-sitter--extract-tool-use server-tool-use-section)))
    (setf (alist-get 'type result) "server_tool_use")
    result))

(defun greger-tree-sitter--extract-server-tool-result (server-tool-result-section)
  "Extract server tool result data from a server tool result section."
  ;; Similar to tool result but with different type
  (let ((result (greger-tree-sitter--extract-tool-result server-tool-result-section)))
    (setf (alist-get 'type result) "web_search_tool_result")
    result))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
