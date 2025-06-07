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
  ;; TODO
  )

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
