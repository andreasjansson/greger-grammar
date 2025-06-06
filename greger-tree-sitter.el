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
    (let ((tree (treesit-parse-string text 'greger)))
      (greger-tree-sitter--extract-dialog tree))))

(defun greger-tree-sitter--extract-dialog (tree)
  "Extract dialog messages from the parsed TREE."
  (let ((root-node (treesit-node-child tree 0))
        (messages '()))

    (message "DEBUG: Root node type: %s" (treesit-node-type root-node))

    ;; Check if we have a source_file or just a section
    (cond
     ((equal (treesit-node-type root-node) "source_file")
      ;; Multiple sections case
      (message "DEBUG: Multiple sections in source_file")
      (let ((child-count (treesit-node-child-count root-node)))
        (dotimes (i child-count)
          (let ((child (treesit-node-child root-node i)))
            (when (equal (treesit-node-type child) "section")
              (when-let ((message (greger-tree-sitter--extract-section child)))
                (push message messages)))))))

     ((equal (treesit-node-type root-node) "section")
      ;; Single section case
      (message "DEBUG: Single section")
      (when-let ((message (greger-tree-sitter--extract-section root-node)))
        (push message messages)))

     (t
      (message "DEBUG: Unknown root node type: %s" (treesit-node-type root-node))))

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
  (let ((content-node (treesit-node-child-by-field-name
                       (treesit-node-child section-node 0) "content")))
    `((role . "user")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-system-section (section-node)
  "Extract system message from SECTION-NODE."
  (let ((content-node (treesit-node-child-by-field-name
                       (treesit-node-child section-node 0) "content")))
    `((role . "system")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-assistant-section (section-node)
  "Extract assistant message from SECTION-NODE."
  (let ((content-node (treesit-node-child-by-field-name
                       (treesit-node-child section-node 0) "content")))
    `((role . "assistant")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-thinking-section (section-node)
  "Extract thinking content and return as assistant message."
  (let ((content-node (treesit-node-child-by-field-name
                       (treesit-node-child section-node 0) "content")))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . ,(if content-node
                                    (greger-tree-sitter--extract-content content-node)
                                  ""))))))))

(defun greger-tree-sitter--extract-tool-use-section (section-node)
  "Extract tool use and return as assistant message."
  (let* ((tool-section (treesit-node-child section-node 0))
         (tool-content (treesit-node-child-by-field-name tool-section "tool_use_content"))
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
         (tool-content (treesit-node-child-by-field-name tool-section "tool_result_content"))
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
    ;; Change role to assistant and type to server_tool_result
    (when result
      (setf (alist-get 'role result) "assistant")
      (let ((content (alist-get 'content result)))
        (when (and content (> (length content) 0))
          (setf (alist-get 'type (car content)) "server_tool_result"))))
    result))

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
