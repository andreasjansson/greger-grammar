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
  (let ((dialog '()))
    (dolist (child (treesit-node-children node))
      (let ((entry (greger-tree-sitter--extract-entry-from-node child)))
        (when entry
          (push entry dialog))))
    (nreverse dialog)))

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
  (let ((name (greger-tree-sitter--extract-tool-name node))
        (id (greger-tree-sitter--extract-tool-id node))
        (params (greger-tree-sitter--extract-tool-params node)))
    `((role . "assistant")
      (content . (((type . "tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-tree-sitter--extract-tool-result-entry (node)
  "Extract tool result entry from NODE."
  (let ((id (greger-tree-sitter--extract-tool-result-id node))
        (content (greger-tree-sitter--extract-tool-result-content node)))
    `((role . "user")
      (content . (((type . "tool_result")
                   (tool_use_id . ,id)
                   (content . ,content)))))))

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

(defun greger-tree-sitter--extract-tool-name (node)
  "Extract tool name from tool use NODE."
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

(defun greger-tree-sitter--extract-tool-id (node)
  "Extract tool ID from tool use NODE."
  (let ((id-node (treesit-node-child-by-field-name node "id")))
    (if id-node
        (treesit-node-text id-node t)
      (let ((children (treesit-node-children node))
            (result nil))
        (while (and children (not result))
          (let ((child (car children)))
            (when (string= (treesit-node-type child) "id")
              (setq result (treesit-node-text child t)))
            (setq children (cdr children))))
        result))))

(defun greger-tree-sitter--extract-tool-params (node)
  "Extract tool parameters from tool use NODE."
  (let ((params '())
        (children (treesit-node-children node)))
    (dolist (child children)
      (when (string= (treesit-node-type child) "tool_param")
        (let ((param-name (greger-tree-sitter--extract-tool-param-name child))
              (param-value (greger-tree-sitter--extract-tool-param-value child)))
          (when (and param-name param-value)
            (push (cons (intern param-name) param-value) params)))))
    (nreverse params)))

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
        (treesit-node-text value-node t)
      (let ((children (treesit-node-children node))
            (result nil))
        (while (and children (not result))
          (let ((child (car children)))
            (when (string= (treesit-node-type child) "value")
              (setq result (treesit-node-text child t)))
            (setq children (cdr children))))
        result))))

(defun greger-tree-sitter--extract-tool-result-id (node)
  "Extract tool result ID from tool_result NODE."
  (let ((id-node (treesit-node-child-by-field-name node "id")))
    (if id-node
        (treesit-node-text id-node t)
      (let ((children (treesit-node-children node)))
        (dolist (child children)
          (when (string= (treesit-node-type child) "id")
            (return (treesit-node-text child t))))))))

(defun greger-tree-sitter--extract-tool-result-content (node)
  "Extract tool result content from tool_result NODE."
  (let ((content-node (treesit-node-child-by-field-name node "content")))
    (if content-node
        (treesit-node-text content-node t)
      (let ((children (treesit-node-children node)))
        (dolist (child children)
          (when (string= (treesit-node-type child) "content")
            (return (treesit-node-text child t))))))))
