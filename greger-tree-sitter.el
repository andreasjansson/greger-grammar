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
            (push server-tool-result-data pending-assistant-content)))

         ((string= section-type "citations_section")
          ;; Extract citations and add as text with citations attached to pending assistant content
          (let ((citations-data (greger-tree-sitter--extract-citations-section section)))
            (dolist (item citations-data)
              (push item pending-assistant-content)))))))

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
                (let ((content-text (treesit-node-text param-child)))
                  ;; Remove the <tool.ID> wrapper using string operations
                  (when (string-match "^<tool\\.[^>]+>" content-text)
                    (let* ((start-tag-end (match-end 0))
                           (remaining-text (substring content-text start-tag-end)))
                      ;; Look for closing tag (including possible whitespace/newlines before it)
                      ;; Use [\s\S] to match any character including newlines
                      (when (string-match "\\([[:ascii:]]*?\\)\\s-*</tool\\.[^>]+>\\s-*$" remaining-text)
                        (setq content-text (match-string 1 remaining-text)))))
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
          (let ((content-text (treesit-node-text child)))
            ;; Remove the <tool.ID> wrapper - note that the closing > might be missing
            ;; Use string operations instead of regex to handle multiline content
            (when (string-match "^<tool\\.[^>]+>" content-text)
              (let* ((start-tag-end (match-end 0))
                     (remaining-text (substring content-text start-tag-end)))
                ;; Look for closing tag (including possible whitespace/newlines before it)
                (when (string-match "\\(.*?\\)\\s-*</tool\\.[^>]+>\\s-*$" remaining-text)
                  (setq content-text (match-string 1 remaining-text)))))
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

(defun greger-tree-sitter--extract-citations-section (citations-section)
  "Extract citations section and return list of text blocks with citations attached."
  (let ((children (treesit-node-children citations-section))
        (cited-text nil)
        (citations '())
        (result '())
        (i 0))

    ;; Process children sequentially
    (while (< i (length children))
      (let* ((child (nth i children))
             (node-type (treesit-node-type child)))
        (cond
         ((string= node-type "text_block")
          ;; Check if this is before a citation_entry (cited text) or after (metadata)
          (let ((next-child (when (< (1+ i) (length children)) (nth (1+ i) children))))
            (if (and next-child (string= (treesit-node-type next-child) "citation_entry"))
                ;; This text_block comes before citation_entry, so it's cited text
                (let ((text (string-trim (treesit-node-text child))))
                  (when (> (length text) 0)
                    (setq cited-text (if cited-text
                                         (concat cited-text " " text)
                                       text))))
              ;; This text_block comes after citation_entry, so it's metadata
              (when (and citations (> (length citations) 0))
                ;; Parse metadata and update the last citation
                (let* ((metadata-text (treesit-node-text child))
                       (lines (split-string metadata-text "\n"))
                       (last-citation (car citations))
                       (title nil)
                       (cited-text-meta nil)
                       (encrypted-index nil))

                  ;; Parse metadata lines
                  (dolist (line lines)
                    (setq line (string-trim line))
                    (when (> (length line) 0)
                      (cond
                       ((string-prefix-p "Title:" line)
                        (setq title (string-trim (substring line 6))))
                       ((string-prefix-p "Cited text:" line)
                        (setq cited-text-meta (string-trim (substring line 11))))
                       ((string-prefix-p "Encrypted index:" line)
                        (setq encrypted-index (string-trim (substring line 16)))))))

                  ;; Update the citation with metadata
                  (setf (alist-get 'title last-citation) title)
                  (setf (alist-get 'cited_text last-citation) cited-text-meta)
                  (setf (alist-get 'encrypted_index last-citation) encrypted-index))))))

         ((string= node-type "citation_entry")
          ;; Extract URL from citation entry
          (let* ((entry-text (treesit-node-text child))
                 (lines (split-string entry-text "\n"))
                 (url-line (and lines (string-trim (car lines))))
                 (url (when (string-prefix-p "###" url-line)
                        (string-trim (substring url-line 3)))))
            (when url
              (push `((type . "web_search_result_location")
                      (url . ,url)
                      (title . nil)
                      (cited_text . nil)
                      (encrypted_index . nil))
                    citations)))))
        (setq i (1+ i))))

    ;; Create result with cited text and citations
    (when cited-text
      (if citations
          (push `((type . "text")
                  (text . ,cited-text)
                  (citations . ,(nreverse citations)))
                result)
        (push `((type . "text")
                (text . ,cited-text))
              result)))

    (nreverse result)))

(defun greger-tree-sitter--parse-citation-entry (citation-entry-node)
  "Parse a citation entry node and return citation data."
  (let ((text (treesit-node-text citation-entry-node)))
    ;; Parse lines: first line has ###, then URL, then key-value pairs
    (let* ((lines (split-string text "\n"))
           (url-line (and lines (string-trim (car lines))))
           (url (when (string-prefix-p "###" url-line)
                  (string-trim (substring url-line 3))))
           (title nil)
           (cited-text nil)
           (encrypted-index nil))

      ;; Parse subsequent lines for metadata
      (dolist (line (cdr lines))
        (setq line (string-trim line))
        (when (> (length line) 0)
          (cond
           ((string-prefix-p "Title:" line)
            (setq title (string-trim (substring line 6))))
           ((string-prefix-p "Cited text:" line)
            (setq cited-text (string-trim (substring line 11))))
           ((string-prefix-p "Encrypted index:" line)
            (setq encrypted-index (string-trim (substring line 16)))))))

      ;; Return citation object if we have a URL
      (when url
        `((type . "web_search_result_location")
          (url . ,url)
          (title . ,title)
          (cited_text . ,cited-text)
          (encrypted_index . ,encrypted-index))))))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
