(require 'treesit)
(require 'cl-lib)

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

(defun greger-tree-sitter--extract-dialog-from-node (node)
  "Extract dialog entries from a tree-sitter NODE."
  (let ((raw-entries '()))
    ;; First extract all entries
    (dolist (child (treesit-node-children node))
      (let ((entry (greger-tree-sitter--extract-entry-from-node child)))
        (when entry
          (push entry raw-entries))))
    ;; Then merge assistant-related entries
    (greger-tree-sitter--merge-assistant-entries (nreverse raw-entries))))

(defun greger-tree-sitter--merge-assistant-entries (entries)
  "Merge consecutive assistant-related entries into single messages."
  (let ((result '())
        (current-assistant-content '()))
    (dolist (entry entries)
      (let ((role (cdr (assoc 'role entry))))
        (cond
         ;; If this is an assistant-type message, accumulate content
         ((string= role "assistant")
          (let ((content (cdr (assoc 'content entry))))
            (cond
             ;; Content is already a list of content blocks
             ((and (listp content) (listp (car content)) (assoc 'type (car content)))
              (setq current-assistant-content
                    (append current-assistant-content content)))
             ;; Content is plain text, convert to text block
             ((stringp content)
              (setq current-assistant-content
                    (append current-assistant-content
                            `(((type . "text") (text . ,content))))))
             ;; Content is some other list format
             (t
              (setq current-assistant-content
                    (append current-assistant-content content))))))
         ;; For non-assistant messages, flush any accumulated assistant content first
         (t
          (when current-assistant-content
            (setq result (greger-tree-sitter--flush-assistant-content current-assistant-content result))
            (setq current-assistant-content '()))
          (push entry result)))))
    ;; Don't forget any remaining assistant content
    (when current-assistant-content
      (setq result (greger-tree-sitter--flush-assistant-content current-assistant-content result)))
    (nreverse result)))

(defun greger-tree-sitter--flush-assistant-content (content result)
  "Flush accumulated assistant CONTENT to RESULT list, returning updated result."
  (if (and (= (length content) 1)
           (string= (cdr (assoc 'type (car content))) "text")
           (not (assoc 'citations (car content))))
      ;; Single text block without citations - use plain text format
      (cons `((role . "assistant")
              (content . ,(cdr (assoc 'text (car content))))) result)
    ;; Multiple blocks or special blocks - use content blocks format
    (cons `((role . "assistant")
            (content . ,content)) result)))

(defun greger-tree-sitter--extract-entry-from-node (node)
  "Extract a single dialog entry from NODE."
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= node-type "user")
      (greger-tree-sitter--extract-user-entry node))
     ((string= node-type "assistant")
      (greger-tree-sitter--extract-assistant-entry node))
     ((string= node-type "system")
      (greger-tree-sitter--extract-system-entry node))
     ((string= node-type "thinking")
      (greger-tree-sitter--extract-thinking-entry node))
     ((string= node-type "tool_use")
      (greger-tree-sitter--extract-tool-use-entry node))
     ((string= node-type "tool_result")
      (greger-tree-sitter--extract-tool-result-entry node))
     ((string= node-type "server_tool_use")
      (greger-tree-sitter--extract-server-tool-use-entry node))
     ((string= node-type "server_tool_result")
      (greger-tree-sitter--extract-server-tool-result-entry node))
     ((string= node-type "citations")
      (greger-tree-sitter--extract-citations-entry node))
     (t nil))))

(defun greger-tree-sitter--extract-user-entry (node)
  "Extract user entry from NODE."
  (let ((content (greger-tree-sitter--extract-text-content node)))
    `((role . "user")
      (content . ,content))))

(defun greger-tree-sitter--extract-system-entry (node)
  "Extract system entry from NODE."
  (let ((content (greger-tree-sitter--extract-text-content node)))
    `((role . "system")
      (content . ,content))))

(defun greger-tree-sitter--extract-thinking-entry (node)
  "Extract thinking entry from NODE."
  (let ((content (greger-tree-sitter--extract-text-content node)))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . ,content)))))))

(defun greger-tree-sitter--extract-assistant-entry (node)
  "Extract assistant entry from NODE."
  (let ((content (greger-tree-sitter--extract-text-content node)))
    `((role . "assistant")
      (content . ,content))))

(defun greger-tree-sitter--extract-tool-use-entry (node)
  "Extract tool use entry from NODE."
  (let ((name nil)
        (id nil)
        (params '()))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "name")
          (setq name (greger-tree-sitter--extract-key child)))
         ((string= child-type "id")
          (setq id (greger-tree-sitter--extract-key child)))
         ((string= child-type "tool_param")
          (push (greger-tree-sitter--extract-tool-param child) params)))))
    (setq params (nreverse params))
    `((role . "assistant")
      (content . (((type . "tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-tree-sitter--extract-key (node)
  (let ((child (treesit-node-child-by-field-name node "value")))
    (string-trim (treesit-node-text child t))))

(defun greger-tree-sitter--extract-tool-param (node)
  (let ((name nil)
        (value nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "name")
          (setq name (intern (string-trim (treesit-node-text child t)))))
         ((string= child-type "value")
          (setq value (greger-tree-sitter--extract-tool-content child))))))
    `(,name . ,value)))

(defun greger-tree-sitter--extract-tool-content (node)
  (let* ((value-node (treesit-node-child-by-field-name node "value"))
         (value (treesit-node-text value-node)))
    (greger-tree-sitter--convert-param-value
     (greger-tree-sitter--strip-single-newlines value))))

(defun greger-tree-sitter--strip-single-newlines (str)
  "Strip a single newline from the front and back of STR."
  (string-trim str "\\`\n?" "\n?\\'"))

(defun greger-tree-sitter--extract-tool-result-entry (node)
  "Extract tool result entry from NODE."
  (let ((id nil)
        (content nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "id")
          (setq id (greger-tree-sitter--extract-key child)))
         ((string= child-type "content")
          (setq content (greger-tree-sitter--extract-xml-content (treesit-node-text child t)))))))
    `((role . "user")
      (content . (((type . "tool_result")
                   (tool_use_id . ,id)
                   (content . ,content)))))))

(defun greger-tree-sitter--extract-server-tool-use-entry (node)
  "Extract server tool use entry from NODE."
  (let ((name nil)
        (id nil)
        (params '()))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "name")
          (setq name (greger-tree-sitter--extract-key child)))
         ((string= child-type "id")
          (setq id (greger-tree-sitter--extract-key child)))
         ((string= child-type "tool_param")
          (push (greger-tree-sitter--extract-tool-param child) params)))))
    (setq params (nreverse params))
    `((role . "assistant")
      (content . (((type . "server_tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-tree-sitter--extract-server-tool-result-entry (node)
  "Extract server tool result entry from NODE."
  (let ((id nil)
        (content nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "id")
          (setq id (greger-tree-sitter--extract-key child)))
         ((string= child-type "content")
          (setq content (greger-tree-sitter--extract-xml-content (treesit-node-text child t)))))))
    ;; Check if this is a web search result and parse accordingly
    (let ((parsed-content (greger-tree-sitter--parse-json-or-plain-content content)))
      `((role . "assistant")
        (content . (((type . ,(if (listp parsed-content) "web_search_tool_result" "server_tool_result"))
                     (tool_use_id . ,id)
                     (content . ,parsed-content))))))))

(defun greger-tree-sitter--extract-citations-entry (node)
  "Extract citations entry from NODE."
  (let ((text-content nil)
        (citation-entries '()))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "text")
          (setq text-content (greger-tree-sitter--extract-text-content child)))
         ((string= child-type "citation_entry")
          (push (greger-tree-sitter--extract-citation-entry child) citation-entries)))))
    (setq citation-entries (nreverse citation-entries))
    ;; Return a result that can be merged with other content
    (if text-content
        ;; Text with citations
        `((role . "assistant")
          (content . (((type . "text")
                       (text . ,text-content)
                       (citations . ,citation-entries)))))
      ;; Just citations (for cases where citations are at the end)
      `((role . "assistant")
        (content . (((type . "text")
                     (citations . ,citation-entries))))))))

(defun greger-tree-sitter--extract-text-content (node)
  "Extract text content from NODE, handling nested structures."
  (let ((result (greger-tree-sitter--collect-text-blocks node "")))
    (string-trim result)))

(defun greger-tree-sitter--collect-text-blocks (node result)
  "Recursively collect text from text nodes in NODE and append to RESULT."
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= node-type "text")
      (concat result (treesit-node-text node t)))
     (t
      (let ((text-result result))
        (dolist (child (treesit-node-children node))
          (setq text-result (greger-tree-sitter--collect-text-blocks child text-result)))
        text-result)))))

(defun greger-tree-sitter--extract-tool-params (node)
  "Extract tool parameters from tool use NODE."
  (let ((params '())
        (children (treesit-node-children node)))
    (dolist (child children)
      (when (string= (treesit-node-type child) "tool_param")
        (let ((param-name (greger-tree-sitter--extract-tool-param-name child))
              (param-value (greger-tree-sitter--extract-tool-param-value child)))
          (when (and param-name param-value)
            (push (cons (intern param-name) (greger-tree-sitter--convert-param-value param-value)) params)))))
    (nreverse params)))

(defun greger-tree-sitter--convert-param-value (value)
  "Convert VALUE to appropriate type (number if it looks like a number, otherwise string)."
  (if (string-match "^[0-9]+$" value)
      (string-to-number value)
    value))

(defun greger-tree-sitter--extract-tool-param-name (node)
  "Extract parameter name from tool_param NODE."
  (let ((name-node (treesit-node-child-by-field-name node "name")))
    (if name-node
        (treesit-node-text name-node t)
      (let ((children (treesit-node-children node))
            (result nil))
        (while (and children (not result))
          (let ((child (car children)))
            (when (string= (treesit-node-type child) "name")
              (setq result (treesit-node-text child t)))
            (setq children (cdr children))))
        result))))

(defun greger-tree-sitter--extract-tool-param-value (node)
  "Extract parameter value from tool_param NODE."
  (let ((value-node (treesit-node-child-by-field-name node "value")))
    (if value-node
        (greger-tree-sitter--extract-xml-content (treesit-node-text value-node t))
      (let ((children (treesit-node-children node))
            (result nil))
        (while (and children (not result))
          (let ((child (car children)))
            (when (string= (treesit-node-type child) "value")
              (setq result (greger-tree-sitter--extract-xml-content (treesit-node-text child t))))
            (setq children (cdr children))))
        result))))

(defun greger-tree-sitter--extract-xml-content (text)
  "Extract content from XML wrapper tags in TEXT."
  (let ((content (if (string-match "^\\s-*<[^>]+>\\s-*\\(\\(?:.\\|\n\\)*?\\)\\s-*</[^>]+>\\s-*$" text)
                     (string-trim (match-string 1 text))
                   (string-trim text))))
    ;; Unescape JSON quotes if this looks like JSON
    (if (and (string-match-p "^\\s-*[{\\[]" content)
             (string-match-p "[}\\]]\\s-*$" content))
        (replace-regexp-in-string "\\\\\"" "\"" content)
      content)))

(defun greger-tree-sitter--parse-json-or-plain-content (content)
  "Parse CONTENT as JSON if it looks like JSON, otherwise return as plain text."
  (if (and (string-match-p "^\\s-*\\[\\|^\\s-*{" content)
           (condition-case nil
               (json-parse-string content :object-type 'alist :array-type 'list)
             (json-parse-error nil)))
      (json-parse-string content :object-type 'alist :array-type 'list)
    content))

(defun greger-tree-sitter--extract-citation-entry (node)
  "Extract a citation entry from NODE."
  (let ((url nil)
        (title nil)
        (cited-text nil)
        (encrypted-index nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "url")
          (setq url (string-trim (treesit-node-text child t))))
         ((string= child-type "title")
          (setq title (string-trim (treesit-node-text child t))))
         ((string= child-type "cited_text")
          (setq cited-text (string-trim (treesit-node-text child t))))
         ((string= child-type "encrypted_index")
          (setq encrypted-index (string-trim (treesit-node-text child t)))))))
    `((type . "web_search_result_location")
      (url . ,url)
      (title . ,title)
      (cited_text . ,cited-text)
      (encrypted_index . ,encrypted-index))))
