(require 'treesit)

(add-to-list 'treesit-extra-load-path "/Users/andreas/scratch/greger-grammar")

(defun greger-tree-sitter-parse (text)
  "Parse greger conversation TEXT using tree-sitter and return structured dialog.

INPUT:
  TEXT - A string containing greger conversation format, e.g.:
    \"## USER:

    Hello, how are you?

    ## ASSISTANT:

    I'm doing well, thanks! <cite>This is cited text</cite>

    ## CITATIONS:

    ### https://example.com

    Title: Example Site
    Cited text: This is cited text from the source
    Encrypted index: abc123\"

OUTPUT:
  Returns a list of message objects, each with 'role and 'content fields:
  - Role is one of: \"user\", \"assistant\", \"system\"
  - Content can be either:
    a) A simple string for basic messages
    b) A list of content blocks for complex messages

  Content blocks have 'type field and additional fields:
  - type=\"text\": Has 'text field, optionally 'citations field
  - type=\"thinking\": Has 'thinking field
  - type=\"tool_use\": Has 'id, 'name, 'input fields
  - type=\"tool_result\": Has 'tool_use_id, 'content fields
  - type=\"server_tool_use\": Has 'id, 'name, 'input fields
  - type=\"web_search_tool_result\": Has 'tool_use_id, 'content fields

EXAMPLE OUTPUT:
  ((role . \"user\") (content . \"Hello, how are you?\"))
  ((role . \"assistant\")
   (content . (((type . \"text\") (text . \"I'm doing well, thanks!\"))
               ((type . \"text\")
                (text . \"This is cited text\")
                (citations . (((type . \"web_search_result_location\")
                               (url . \"https://example.com\")
                               (title . \"Example Site\")
                               (cited_text . \"This is cited text from the source\")
                               (encrypted_index . \"abc123\"))))))))

ERRORS:
  Throws an error if tree-sitter greger parser is not available."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let ((parser (treesit-parser-create 'greger)))
      (greger-tree-sitter--extract-dialog parser text))))

(defun greger-tree-sitter--extract-dialog (parser text)
  "Extract dialog structure from parsed greger conversation."
  (let* ((root-node (treesit-parser-root-node parser))
         (sections (treesit-node-children root-node))
         (dialog '()))

    (dolist (section sections)
      (let ((section-type (treesit-node-type section)))
        (cond
         ((string= section-type "user_section")
          (push (greger-tree-sitter--extract-user-section section) dialog))
         ((string= section-type "assistant_section")
          (push (greger-tree-sitter--extract-assistant-section section) dialog))
         ((string= section-type "system_section")
          (push (greger-tree-sitter--extract-system-section section) dialog))
         ((string= section-type "thinking_section")
          (push (greger-tree-sitter--extract-thinking-section section) dialog)))))

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

(defun greger-tree-sitter--extract-thinking-section (section-node)
  "Extract thinking section and convert to assistant content with thinking type."
  (let ((thinking-content (greger-tree-sitter--extract-section-text section-node)))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . ,thinking-content)))))))

(defun greger-tree-sitter--extract-section-text (section-node)
  "Extract text content from a section node."
  (let ((text-blocks (treesit-query-capture
                     section-node
                     '((text_block) @text))))
    (if text-blocks
        (string-trim
         (mapconcat (lambda (capture)
                      (treesit-node-text (cdr capture)))
                    text-blocks ""))
      "")))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
