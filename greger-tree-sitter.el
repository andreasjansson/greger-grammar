;;; greger-tree-sitter.el --- Tree-sitter integration for greger format -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides tree-sitter parsing for the greger conversation format.
;; It can be used as a replacement for the regex-based parser in greger.el.

;;; Code:

(require 'treesit)

(add-to-list 'treesit-extra-load-path "/Users/andreas/scratch/greger-grammar")

(defun greger-tree-sitter-parse (text)
  "Parse TEXT using tree-sitter and return the dialog structure.
Returns the same format as `greger-parser-parse-dialog-messages-only'."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let ((parser (treesit-parser-create 'greger)))
      (greger-tree-sitter--extract-dialog parser text))))

(defun greger-tree-sitter--extract-dialog (parser text)
  "Extract dialog messages from the parsed PARSER with TEXT."
  (let ((root-node (treesit-parser-root-node parser))
        (messages '())
        (pending-citations nil))

    ;; Check if we have a source_file or just a section
    (cond
     ((equal (treesit-node-type root-node) "source_file")
      ;; Multiple sections case - process sections and handle citations
      (let ((sections (greger-tree-sitter--get-all-sections root-node)))
        (setq messages (greger-tree-sitter--process-sections-with-citations sections))))

     ((equal (treesit-node-type root-node) "section")
      ;; Single section case
      (when-let ((message (greger-tree-sitter--extract-section root-node)))
        (push message messages)))

     (t
      (error "Unknown root node type: %s" (treesit-node-type root-node))))

    (nreverse messages)))

(defun greger-tree-sitter--get-sections (root-node)
  "Get all section nodes from ROOT-NODE."
  (let ((sections '())
        (child-count (treesit-node-child-count root-node)))
    (dotimes (i child-count)
      (let ((child (treesit-node-child root-node i)))
        (when (equal (treesit-node-type child) "section")
          (push child sections))))
    (nreverse sections)))

(defun greger-tree-sitter--extract-section (section-node)
  "Extract a dialog message from a SECTION-NODE."
  (let ((section-type (treesit-node-type (treesit-node-child section-node 0))))
    (cond
     ((equal section-type "user_section")
      (greger-tree-sitter--extract-user-section section-node))
     ((equal section-type "system_section")
      (greger-tree-sitter--extract-system-section section-node))
     ((equal section-type "assistant_section")
      (greger-tree-sitter--extract-assistant-section section-node))
     ((equal section-type "thinking_section")
      (greger-tree-sitter--extract-thinking-section section-node))
     ((equal section-type "tool_use_section")
      (greger-tree-sitter--extract-tool-use-section section-node))
     ((equal section-type "tool_result_section")
      (greger-tree-sitter--extract-tool-result-section section-node))
     ((equal section-type "server_tool_use_section")
      (greger-tree-sitter--extract-server-tool-use-section section-node))
     ((equal section-type "server_tool_result_section")
      (greger-tree-sitter--extract-server-tool-result-section section-node))
     (t nil))))

(defun greger-tree-sitter--extract-user-section (section-node)
  "Extract user message from SECTION-NODE."
  (let* ((user-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type user-section-node "section_content")))

    `((role . "user")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-system-section (section-node)
  "Extract system message from SECTION-NODE."
  (let* ((system-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type system-section-node "section_content")))
    `((role . "system")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-assistant-section (section-node)
  "Extract assistant message from SECTION-NODE."
  (let* ((assistant-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type assistant-section-node "section_content")))
    `((role . "assistant")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-thinking-section (section-node)
  "Extract thinking content and return as assistant message."
  (let* ((thinking-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type thinking-section-node "section_content")))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . ,(if content-node
                                    (greger-tree-sitter--extract-content content-node)
                                  ""))))))))

(defun greger-tree-sitter--extract-tool-use-section (section-node)
  "Extract tool use and return as assistant message."
  (let* ((tool-section (treesit-node-child section-node 0))
         (tool-content (greger-tree-sitter--find-child-by-type tool-section "tool_use_content"))
         (tool-name nil)
         (tool-id nil)
         (parameters '()))

    ;; Extract tool name, ID, and parameters
    (when tool-content
      (let ((child-count (treesit-node-child-count tool-content)))
        (dotimes (i child-count)
          (let ((child (treesit-node-child tool-content i)))
            (cond
             ((equal (treesit-node-type child) "tool_name_line")
              (setq tool-name (treesit-node-text
                               (treesit-node-child-by-field-name child "name"))))
             ((equal (treesit-node-type child) "tool_id_line")
              (setq tool-id (treesit-node-text
                             (treesit-node-child-by-field-name child "id"))))
             ((equal (treesit-node-type child) "tool_parameter")
              (let ((param-name (treesit-node-text
                                 (treesit-node-child-by-field-name child "param_name")))
                    (param-value (greger-tree-sitter--extract-tool-param-value child)))
                (push (cons (intern param-name) param-value) parameters))))))))

    `((role . "assistant")
      (content . (((type . "tool_use")
                   (id . ,tool-id)
                   (name . ,tool-name)
                   (input . ,(nreverse parameters))))))))

(defun greger-tree-sitter--extract-tool-param-value (param-node)
  "Extract the value from a tool parameter node."
  (let ((param-block (treesit-node-child-by-field-name param-node "param_value")))
    (if param-block
        (let ((content-node (treesit-node-child-by-field-name param-block "content")))
          (if content-node
              (string-trim (treesit-node-text content-node))
            ""))
      "")))

(defun greger-tree-sitter--extract-tool-result-section (section-node)
  "Extract tool result and return as user message."
  (let* ((tool-section (treesit-node-child section-node 0))
         (tool-content (greger-tree-sitter--find-child-by-type tool-section "tool_result_content"))
         (tool-id nil)
         (result-content ""))

    ;; Extract tool ID and result content
    (when tool-content
      (let ((child-count (treesit-node-child-count tool-content)))
        (dotimes (i child-count)
          (let ((child (treesit-node-child tool-content i)))
            (cond
             ((equal (treesit-node-type child) "tool_result_id_line")
              (setq tool-id (treesit-node-text
                             (treesit-node-child-by-field-name child "id"))))
             ((equal (treesit-node-type child) "tool_result_block")
              (let ((content-node (treesit-node-child-by-field-name child "content")))
                (when content-node
                  (setq result-content (string-trim (treesit-node-text content-node)))))))))))

    `((role . "user")
      (content . (((type . "tool_result")
                   (tool_use_id . ,tool-id)
                   (content . ,result-content)))))))

(defun greger-tree-sitter--extract-server-tool-use-section (section-node)
  "Extract server tool use and return as assistant message."
  ;; Similar to tool_use but with server_tool_use type
  (let ((result (greger-tree-sitter--extract-tool-use-section section-node)))
    ;; Change the type to server_tool_use
    (when result
      (let ((content (alist-get 'content result)))
        (when (and content (> (length content) 0))
          (setf (alist-get 'type (car content)) "server_tool_use"))))
    result))

(defun greger-tree-sitter--extract-server-tool-result-section (section-node)
  "Extract server tool result and return as assistant message."
  (let ((result (greger-tree-sitter--extract-tool-result-section section-node)))
    ;; Change role to assistant and type to web_search_tool_result
    (when result
      (setf (alist-get 'role result) "assistant")
      (let ((content (alist-get 'content result)))
        (when (and content (> (length content) 0))
          (setf (alist-get 'type (car content)) "web_search_tool_result"))))
    result))

(defun greger-tree-sitter--find-child-by-type (node type)
  "Find the first child of NODE with the given TYPE."
  (let ((child-count (treesit-node-child-count node))
        (found nil))
    (dotimes (i child-count)
      (let ((child (treesit-node-child node i)))
        (when (and (not found) (equal (treesit-node-type child) type))
          (setq found child))))
    found))

(defun greger-tree-sitter--get-all-sections (root-node)
  "Get all section nodes from ROOT-NODE."
  (let ((sections '())
        (child-count (treesit-node-child-count root-node)))
    (dotimes (i child-count)
      (let ((child (treesit-node-child root-node i)))
        (when (equal (treesit-node-type child) "section")
          (push child sections))))
    (nreverse sections)))

(defun greger-tree-sitter--process-sections-with-citations (sections)
  "Process SECTIONS and handle citation associations."
  (let ((messages '())
        (current-assistant-blocks '())
        (i 0))

    (while (< i (length sections))
      (let* ((section (nth i sections))
             (section-type (greger-tree-sitter--get-section-type section)))

        (cond
         ;; User section - flush any pending assistant blocks and add user message
         ((equal section-type "user_section")
          (when current-assistant-blocks
            (push `((role . "assistant") (content . ,(nreverse current-assistant-blocks))) messages)
            (setq current-assistant-blocks '()))
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message (push message messages))))

         ;; System section - flush any pending assistant blocks and add system message
         ((equal section-type "system_section")
          (when current-assistant-blocks
            (push `((role . "assistant") (content . ,(nreverse current-assistant-blocks))) messages)
            (setq current-assistant-blocks '()))
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message (push message messages))))

         ;; Assistant-related sections - collect content blocks
         ((member section-type '("assistant_section" "thinking_section" "tool_use_section"
                                 "server_tool_use_section" "server_tool_result_section"))
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message
              (let ((content (alist-get 'content message)))
                (if (listp content)
                    ;; Add all content blocks
                    (setq current-assistant-blocks (append current-assistant-blocks content))
                  ;; Convert string content to text block
                  (when (and (stringp content) (> (length (string-trim content)) 0))
                    (push `((type . "text") (text . ,content)) current-assistant-blocks)))))))

         ;; Citations section - associate with pending assistant blocks
         ((equal section-type "citations_section")
          (when current-assistant-blocks
            (let ((citations (greger-tree-sitter--extract-citations-section section)))
              ;; Find the last text block that might have cite tags
              (setq current-assistant-blocks
                    (greger-tree-sitter--associate-citations-with-blocks current-assistant-blocks citations)))))

         ;; Tool result section - add as user message
         ((equal section-type "tool_result_section")
          (when current-assistant-blocks
            (push `((role . "assistant") (content . ,(nreverse current-assistant-blocks))) messages)
            (setq current-assistant-blocks '()))
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message (push message messages))))))

      (setq i (1+ i)))

    ;; Flush any remaining assistant blocks
    (when current-assistant-blocks
      (push `((role . "assistant") (content . ,(nreverse current-assistant-blocks))) messages))

    (nreverse messages)))

(defun greger-tree-sitter--get-section-type (section-node)
  "Get the type of a section node."
  (treesit-node-type (treesit-node-child section-node 0)))

(defun greger-tree-sitter--associate-citations-with-blocks (content-blocks citations)
  "Associate CITATIONS with content blocks that have cite tags."
  (let ((result '()))
    (dolist (block content-blocks)
      (if (and (equal (alist-get 'type block) "text")
               (string-match-p "<cite>" (alist-get 'text block "")))
          ;; This text block has cite tags - parse it with citations
          (let ((parsed-blocks (greger-tree-sitter--parse-content-with-citations
                                (alist-get 'text block) citations)))
            (setq result (append result parsed-blocks)))
        ;; Regular block - add as is
        (push block result)))
    (nreverse result)))

(defun greger-tree-sitter--extract-citations-section (section-node)
  "Extract citations from a citations section."
  (let* ((citations-section (treesit-node-child section-node 0))
         (citations-content (greger-tree-sitter--find-child-by-type citations-section "citations_content"))
         (citations '()))

    (when citations-content
      (let ((child-count (treesit-node-child-count citations-content)))
        (dotimes (i child-count)
          (let ((child (treesit-node-child citations-content i)))
            (when (equal (treesit-node-type child) "citation_entry")
              (push (greger-tree-sitter--extract-citation-entry child) citations))))))

    (nreverse citations)))

(defun greger-tree-sitter--extract-citation-entry (entry-node)
  "Extract a single citation entry."
  (let ((url-node (treesit-node-child-by-field-name entry-node "url"))
        (title nil)
        (cited-text nil)
        (encrypted-index nil))

    (let ((url (if url-node (treesit-node-text url-node) "")))
      (let ((child-count (treesit-node-child-count entry-node)))
        (dotimes (i child-count)
          (let ((child (treesit-node-child entry-node i)))

            (cond
             ((equal (treesit-node-type child) "citation_title")
              (let ((title-node (treesit-node-child-by-field-name child "title")))

                (if title-node
                    (progn
                      (setq title (string-trim (treesit-node-text title-node)))
                      )
                  ;; Fallback: try to extract from the text after "Title:"
                  (let ((text (treesit-node-text child)))
                    (when (string-match "Title:[ \t]*\\(.*\\)" text)
                      (setq title (string-trim (match-string 1 text)))
                      )))))
             ((equal (treesit-node-type child) "citation_text")
              (let ((text-node (treesit-node-child-by-field-name child "text")))

                (if text-node
                    (progn
                      (setq cited-text (string-trim (treesit-node-text text-node)))
                      (message "Set cited-text to: %S" cited-text))
                  ;; Fallback: try to extract from the text after "Cited text:"
                  (let ((text (treesit-node-text child)))
                    (when (string-match "Cited text:[ \t]*\\(.*\\)" text)
                      (setq cited-text (string-trim (match-string 1 text)))
                      (message "Extracted cited-text from text: %S" cited-text))))))
             ((equal (treesit-node-type child) "citation_index")
              (let ((index-node (treesit-node-child-by-field-name child "index")))
                (message "Found citation_index, index-node: %S" index-node)
                (if index-node
                    (progn
                      (setq encrypted-index (string-trim (treesit-node-text index-node)))
                      (message "Set encrypted-index to: %S" encrypted-index))
                  ;; Fallback: try to extract from the text after "Encrypted index:"
                  (let ((text (treesit-node-text child)))
                    (when (string-match "Encrypted index:[ \t]*\\(.*\\)" text)
                      (setq encrypted-index (string-trim (match-string 1 text)))
                      (message "Extracted encrypted-index from text: %S" encrypted-index))))))))))

      (message "Final citation: url=%S title=%S cited-text=%S encrypted-index=%S"
               url title cited-text encrypted-index)
      `((type . "web_search_result_location")
        (url . ,url)
        (title . ,title)
        (cited_text . ,cited-text)
        (encrypted_index . ,encrypted-index)))))

(defun greger-tree-sitter--message-has-cite-tags (message)
  "Check if MESSAGE content contains cite tags."
  (let ((content (alist-get 'content message)))
    (and (stringp content)
         (string-match-p "<cite>" content))))

(defun greger-tree-sitter--associate-citations (message citations)
  "Associate CITATIONS with cited text in MESSAGE."
  ;; This is complex - we need to parse the content and split it by cite tags
  (let ((content (alist-get 'content message)))
    (when (stringp content)
      (setf (alist-get 'content message)
            (greger-tree-sitter--parse-content-with-citations content citations))))
  message)

(defun greger-tree-sitter--parse-content-with-citations (content citations)
  "Parse CONTENT and create content blocks with citations."
  (let ((parts '())
        (current-pos 0))

    ;; Find all cite tags and split content accordingly
    (while (string-match "<cite>\\(.*?\\)</cite>" content current-pos)
      (let ((before-cite (substring content current-pos (match-beginning 0)))
            (cite-text (match-string 1 content))
            (after-match (match-end 0)))

        ;; Add text before cite if any
        (when (> (length (string-trim before-cite)) 0)
          (push `((type . "text") (text . ,(string-trim before-cite))) parts))

        ;; Add cited text with citations
        (push `((type . "text")
                (text . ,cite-text)
                (citations . ,citations)) parts)

        (setq current-pos after-match)))

    ;; Add remaining text after last cite
    (let ((remaining (substring content current-pos)))
      (when (> (length (string-trim remaining)) 0)
        (push `((type . "text") (text . ,(string-trim remaining))) parts)))

    (nreverse parts)))

(defun greger-tree-sitter--extract-content (content-node)
  "Extract plain text content from CONTENT-NODE."
  (if content-node
      (string-trim (treesit-node-text content-node))
    ""))

;; Test function
(defun greger-tree-sitter-test ()
  "Test the tree-sitter parser with a simple example."
  (interactive)
  (let ((test-text "## USER:

Hello, how are you?

## ASSISTANT:

Hi there! How can I help you today?"))
    (message "Parsed result: %S" (greger-tree-sitter-parse test-text))))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
