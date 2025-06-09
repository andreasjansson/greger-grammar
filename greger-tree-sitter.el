;;; greger-tree-sitter.el --- Tree-sitter parser for greger dialog format -*- lexical-binding: t -*-

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
  (let ((sections (treesit-node-children root-node))
        (dialog '())
        (pending-assistant-parts '()))

    (dolist (section sections)
      (let ((section-type (treesit-node-type section)))
        (cond
         ;; User sections - flush pending assistant content first
         ((string= section-type "user")
          (when pending-assistant-parts
            (push (greger-tree-sitter--create-assistant-message pending-assistant-parts) dialog)
            (setq pending-assistant-parts '()))
          (push (greger-tree-sitter--extract-user-section section) dialog))

         ;; System sections - flush pending assistant content first
         ((string= section-type "system")
          (when pending-assistant-parts
            (push (greger-tree-sitter--create-assistant-message pending-assistant-parts) dialog)
            (setq pending-assistant-parts '()))
          (push (greger-tree-sitter--extract-system-section section) dialog))

         ;; Assistant sections - add as text to pending parts
         ((string= section-type "assistant")
          (let ((text (greger-tree-sitter--extract-text-content section)))
            (when (> (length (string-trim text)) 0)
              (push `((type . "text") (text . ,text)) pending-assistant-parts))))

         ;; Thinking sections - add to pending assistant parts
         ((string= section-type "thinking")
          (let ((thinking-text (greger-tree-sitter--extract-text-content section)))
            (push `((type . "thinking") (thinking . ,thinking-text)) pending-assistant-parts)))

         ;; Tool use sections - add to pending assistant parts
         ((string= section-type "tool_use")
          (let ((tool-use-data (greger-tree-sitter--extract-tool-use section)))
            (push tool-use-data pending-assistant-parts)))

         ;; Server tool use sections - add to pending assistant parts
         ((string= section-type "server_tool_use")
          (let ((server-tool-use-data (greger-tree-sitter--extract-server-tool-use section)))
            (push server-tool-use-data pending-assistant-parts)))

         ;; Tool result sections - flush assistant, then add as user message
         ((string= section-type "tool_result")
          (when pending-assistant-parts
            (push (greger-tree-sitter--create-assistant-message pending-assistant-parts) dialog)
            (setq pending-assistant-parts '()))
          (let ((tool-result-data (greger-tree-sitter--extract-tool-result section)))
            (push `((role . "user") (content . (,tool-result-data))) dialog)))

         ;; Server tool result sections - add to pending assistant parts (they stay with assistant)
         ((string= section-type "server_tool_result")
          (let ((server-tool-result-data (greger-tree-sitter--extract-server-tool-result section)))
            (push server-tool-result-data pending-assistant-parts)))

         ;; Citations sections - add to pending assistant parts
         ((string= section-type "citations")
          (let ((citations-parts (greger-tree-sitter--extract-citations-section section)))
            (dolist (part citations-parts)
              (push part pending-assistant-parts)))))))

    ;; Flush any remaining pending assistant content
    (when pending-assistant-parts
      (push (greger-tree-sitter--create-assistant-message pending-assistant-parts) dialog))

    (nreverse dialog)))

(defun greger-tree-sitter--create-assistant-message (parts)
  "Create an assistant message from a list of content parts."
  (let ((reversed-parts (nreverse parts)))
    (if (and (= (length reversed-parts) 1)
             (equal (alist-get 'type (car reversed-parts)) "text"))
        ;; Single text part - use simple string content
        `((role . "assistant") (content . ,(alist-get 'text (car reversed-parts))))
      ;; Multiple parts or non-text - use structured content
      `((role . "assistant") (content . ,reversed-parts)))))

(defun greger-tree-sitter--extract-user-section (section-node)
  "Extract user section content."
  (let ((content (greger-tree-sitter--extract-content-blocks section-node)))
    `((role . "user") (content . ,content))))

(defun greger-tree-sitter--extract-system-section (section-node)
  "Extract system section content."
  (let ((content (greger-tree-sitter--extract-content-blocks section-node)))
    `((role . "system") (content . ,content))))

(defun greger-tree-sitter--extract-content-blocks (section-node)
  "Extract content from a section node by finding content_blocks child."
  (let ((children (treesit-node-children section-node))
        (content-text ""))
    ;; Look for content_blocks node
    (dolist (child children)
      (when (string= (treesit-node-type child) "content_blocks")
        (setq content-text (treesit-node-text child))))
    (string-trim content-text)))

(defun greger-tree-sitter--extract-tool-use (tool-use-node)
  "Extract tool use data from a tool use section."
  (let ((children (treesit-node-children tool-use-node))
        (name nil)
        (id nil)
        (input '()))

    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "name")
          (setq name (string-trim (treesit-node-text child))))
         ((string= node-type "id")
          (setq id (string-trim (treesit-node-text child))))
         ((string= node-type "tool_param")
          (let ((param-data (greger-tree-sitter--extract-tool-param child)))
            (when param-data
              (push param-data input)))))))

    `((type . "tool_use")
      (id . ,id)
      (name . ,name)
      (input . ,(nreverse input)))))

(defun greger-tree-sitter--extract-server-tool-use (server-tool-use-node)
  "Extract server tool use data from a server tool use section."
  (let ((tool-use-data (greger-tree-sitter--extract-tool-use server-tool-use-node)))
    (setf (alist-get 'type tool-use-data) "server_tool_use")
    tool-use-data))

(defun greger-tree-sitter--extract-tool-param (param-node)
  "Extract parameter name and value from a tool_param node."
  (let ((children (treesit-node-children param-node))
        (param-name nil)
        (param-value nil))

    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "name")
          (setq param-name (string-trim (treesit-node-text child))))
         ((string= node-type "value")
          (setq param-value (string-trim (treesit-node-text child)))
          ;; Try to convert to number if it looks like one
          (when (string-match "^[0-9]+$" param-value)
            (setq param-value (string-to-number param-value)))))))

    (when (and param-name param-value)
      (cons (intern param-name) param-value))))

(defun greger-tree-sitter--extract-tool-result (tool-result-node)
  "Extract tool result data from a tool result section."
  (let ((children (treesit-node-children tool-result-node))
        (tool-use-id nil)
        (content nil))

    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "id")
          (setq tool-use-id (string-trim (treesit-node-text child))))
         ((string= node-type "content")
          (setq content (string-trim (treesit-node-text child)))))))

    `((type . "tool_result")
      (tool_use_id . ,tool-use-id)
      (content . ,content))))

(defun greger-tree-sitter--extract-server-tool-result (server-tool-result-node)
  "Extract server tool result data from a server tool result section."
  (let ((children (treesit-node-children server-tool-result-node))
        (tool-use-id nil)
        (content nil))

    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "id")
          (setq tool-use-id (string-trim (treesit-node-text child))))
         ((string= node-type "content")
          (setq content (string-trim (treesit-node-text child)))))))

    ;; Determine if this should be web_search_tool_result or server_tool_result
    ;; by checking if there are citations in the parent conversation
    (let ((result-type (if (greger-tree-sitter--has-citations-in-context server-tool-result-node)
                           "web_search_tool_result"
                         "server_tool_result")))

      `((type . ,result-type)
        (tool_use_id . ,tool-use-id)
        (content . ,content)))))

(defun greger-tree-sitter--has-citations-in-context (node)
  "Check if there are any citations sections in the same conversation context."
  (let ((root-node (treesit-node-parent node)))
    ;; Walk up to find the root
    (while (treesit-node-parent root-node)
      (setq root-node (treesit-node-parent root-node)))

    ;; Check if any child of root is a citations section
    (let ((has-citations nil))
      (dolist (child (treesit-node-children root-node))
        (when (string= (treesit-node-type child) "citations")
          (setq has-citations t)))
      has-citations)))

(defun greger-tree-sitter--extract-citations-section (citations-node)
  "Extract citations section and return list of text parts with citations."
  (let ((children (treesit-node-children citations-node))
        (text-parts '())
        (current-citations '())
        (current-text nil))

    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "text")
          ;; Accumulate text
          (let ((text-content (string-trim (treesit-node-text child))))
            (when (> (length text-content) 0)
              (setq current-text (if current-text
                                     (concat current-text " " text-content)
                                   text-content)))))

         ((string= node-type "citation_entry")
          ;; Extract citation data
          (let ((citation-data (greger-tree-sitter--extract-citation-entry child)))
            (when citation-data
              (push citation-data current-citations)))))))

    ;; Create text parts based on what we found
    (cond
     ;; Both text and citations
     ((and current-text current-citations)
      (push `((type . "text")
              (text . ,current-text)
              (citations . ,(nreverse current-citations)))
            text-parts))

     ;; Only text, no citations
     (current-text
      (push `((type . "text")
              (text . ,current-text))
            text-parts))

     ;; Only citations, no text
     (current-citations
      (push `((type . "text")
              (citations . ,(nreverse current-citations)))
            text-parts)))

    (nreverse text-parts)))

(defun greger-tree-sitter--extract-citation-entry (citation-entry-node)
  "Extract citation entry data from a citation entry node."
  (let ((children (treesit-node-children citation-entry-node))
        (url nil)
        (title nil)
        (cited-text nil)
        (encrypted-index nil))

    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ((string= node-type "url")
          (setq url (string-trim (treesit-node-text child))))
         ((string= node-type "title")
          (setq title (string-trim (treesit-node-text child))))
         ((string= node-type "cited_text")
          (setq cited-text (string-trim (treesit-node-text child))))
         ((string= node-type "encrypted_index")
          (setq encrypted-index (string-trim (treesit-node-text child)))))))

    (when url
      `((type . "web_search_result_location")
        (url . ,url)
        (title . ,title)
        (cited_text . ,cited-text)
        (encrypted_index . ,encrypted-index)))))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
