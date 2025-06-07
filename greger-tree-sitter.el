;;; greger-tree-sitter.el --- Tree-sitter integration for greger format -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides tree-sitter parsing for the greger conversation format.
;; It parses markdown-like conversation logs with sections like ## USER:, ## ASSISTANT:,
;; ## TOOL USE:, etc., and converts them to structured dialog messages.
;;
;; MAIN ENTRY POINT:
;;   `greger-tree-sitter-parse' - Parse greger text and return message list
;;
;; KEY FEATURES:
;; - User/Assistant/System message parsing
;; - Tool use workflows (## TOOL USE: → ## TOOL RESULT:)
;; - Server tool workflows (## SERVER TOOL USE: → ## SERVER TOOL RESULT:)
;; - Citations support (<cite>text</cite> + ## CITATIONS: sections)
;; - Thinking sections (## THINKING:)
;; - Complex content blocks with proper ordering
;;
;; CITATION WORKFLOW:
;; When text contains <cite>cited text</cite>, the parser looks for subsequent
;; ## CITATIONS: sections and associates the citation metadata with the cited text.
;; This creates structured content blocks where cited text includes citation objects.
;;
;; OUTPUT FORMAT:
;; Returns list of message objects compatible with greger.el:
;;   ((role . "user") (content . "string content"))
;;   ((role . "assistant") (content . (content-block-list)))
;;
;; Content blocks have 'type and type-specific fields:
;;   - text: 'text field, optional 'citations field
;;   - thinking: 'thinking field
;;   - tool_use: 'id, 'name, 'input fields
;;   - tool_result: 'tool_use_id, 'content fields
;;   - server_tool_use: 'id, 'name, 'input fields
;;   - web_search_tool_result: 'tool_use_id, 'content fields
;;
;; INTERNAL ARCHITECTURE:
;; - Section extraction functions handle each ## HEADER: type
;; - Citation processing associates ## CITATIONS: with <cite> tags
;; - Content block ordering ensures tools → results → text flow
;; - Tree-sitter grammar provides robust parsing of complex structures

;;; Code:

(require 'treesit)
(require 'cl-lib)

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
  "Extract dialog messages from the parsed tree-sitter PARSER with original TEXT.

INPUT:
  PARSER - A tree-sitter parser object that has parsed the greger text
  TEXT - The original text string (currently unused but kept for compatibility)

PROCESSING:
  1. Gets the root node from the parser
  2. Handles two cases:
     - source_file: Multiple sections (full conversation)
     - section: Single section (partial conversation)
  3. For multiple sections, transforms them according to citation requirements
  4. For single section, extracts just that section

OUTPUT:
  Returns a list of message objects in the same format as greger-tree-sitter-parse.
  Messages are returned in the order they appear in the input text.

INTERNAL FUNCTION: This is called by greger-tree-sitter-parse and not intended
for direct use."
  (let ((root-node (treesit-parser-root-node parser))
        (messages '()))

    ;; Check if we have a source_file or just a section
    (cond
     ((equal (treesit-node-type root-node) "source_file")
      ;; Multiple sections case - process sections with citation handling
      (let ((sections (greger-tree-sitter--get-all-sections root-node)))
        (setq messages (greger-tree-sitter--process-sections-with-citations sections))))

     ((equal (treesit-node-type root-node) "section")
      ;; Single section case
      (when-let ((message (greger-tree-sitter--extract-section root-node)))
        (push message messages)))

     (t
      (error "Unknown root node type: %s" (treesit-node-type root-node))))

    messages))

(defun greger-tree-sitter--finalize-assistant-content (blocks)
  "Finalize assistant content BLOCKS, returning either a simple string or content blocks.

INPUT:
  BLOCKS - List of content block objects accumulated for an assistant message

PROCESSING:
  1. Converts citations_with_text and citations_without_text blocks to expected format
  2. Decides whether to return simple string content or structured content blocks:
     - If there's only one text block with no citations → return simple string
     - If there are multiple blocks, or citations, or non-text blocks → return content blocks
  3. Reorders blocks appropriately when returning content blocks

OUTPUT:
  Returns either:
  - A simple string (for basic assistant responses)
  - A list of content blocks (for complex responses with tools, thinking, citations)

LOGIC:
  Simple string: Single text block, no citations, no other block types
  Content blocks: Everything else (multiple blocks, citations, tools, thinking)

INTERNAL FUNCTION: Used by greger-tree-sitter--process-sections-with-citations
to determine the final format of assistant content."
  (let ((converted-blocks (greger-tree-sitter--convert-citation-blocks blocks)))
    (cond
     ;; No blocks - return empty string
     ((null converted-blocks) "")

     ;; Single text block with no citations - return simple string
     ((and (= (length converted-blocks) 1)
           (equal (alist-get 'type (car converted-blocks)) "text")
           (not (alist-get 'citations (car converted-blocks))))
      (alist-get 'text (car converted-blocks)))

     ;; Multiple blocks or complex content - return content blocks
     (t
      (greger-tree-sitter--reorder-assistant-blocks converted-blocks)))))

(defun greger-tree-sitter--convert-citation-blocks (blocks)
  "Convert citations_with_text and citations_without_text blocks to expected format.

INPUT:
  BLOCKS - List of content blocks that may include citations_with_text or citations_without_text

PROCESSING:
  Converts the new citation block types to the format expected by greger.el:
  - citations_with_text → text block with citations field
  - citations_without_text → leave as-is (this shouldn't appear in final output)
  - Other blocks → pass through unchanged

OUTPUT:
  Returns list of blocks in the expected format for greger.el compatibility.

INTERNAL FUNCTION: Used to maintain backward compatibility with existing greger.el
expectations while allowing the tree-sitter parser to be smarter about citations."
  (let ((result '()))
    (dolist (block blocks)
      (let ((type (alist-get 'type block)))
        (cond
         ((equal type "citations_with_text")
          ;; Convert to text block with citations field
          (let ((text (alist-get 'text block))
                (entries (alist-get 'entries block)))
            (push `((type . "text")
                    (text . ,text)
                    (citations . ,entries)) result)))

         ((equal type "citations_without_text")
          ;; This shouldn't typically appear in final assistant content,
          ;; but if it does, we can ignore it or convert it to something else
          ;; For now, skip it as it represents standalone citations
          nil)

         (t
          ;; Pass through other block types unchanged
          (push block result)))))
    (nreverse result)))

(defun greger-tree-sitter--reorder-assistant-blocks (blocks)
  "Reorder assistant content BLOCKS to match expected greger format order.

INPUT:
  BLOCKS - A list of content block objects, each with a 'type field

PROCESSING:
  Categorizes blocks by type and reorders them to match the expected format:
  1. server_tool_use blocks (tool calls made by assistant)
  2. web_search_tool_result blocks (results from server tools)
  3. All other blocks (text, thinking, etc.) in original order

OUTPUT:
  Returns the same list of blocks but reordered. This ensures that tool
  usage appears before tool results, which appears before response text,
  matching the expected conversation flow.

INTERNAL FUNCTION: Used during assistant message processing to ensure
proper block ordering for compatibility with greger.el expectations."
  (let ((server-tool-use '())
        (web-search-tool-result '())
        (other-blocks '()))

    ;; Categorize blocks
    (dolist (block blocks)
      (let ((type (alist-get 'type block)))
        (cond
         ((equal type "server_tool_use")
          (push block server-tool-use))
         ((equal type "web_search_tool_result")
          (push block web-search-tool-result))
         (t
          (push block other-blocks)))))

    ;; Return in desired order: server_tool_use, web_search_tool_result, others
    (append (nreverse server-tool-use)
            (nreverse web-search-tool-result)
            (nreverse other-blocks))))

(defun greger-tree-sitter--get-sections (root-node)
  "Extract all section nodes from ROOT-NODE tree.

INPUT:
  ROOT-NODE - A tree-sitter node representing the root of a parsed document

PROCESSING:
  Iterates through all child nodes of the root and collects those with
  type \"section\". Sections correspond to ## USER:, ## ASSISTANT:, etc.

OUTPUT:
  Returns a list of tree-sitter section nodes in the order they appear
  in the document.

INTERNAL FUNCTION: Helper for extracting sections from parsed trees.
Note: This function is similar to greger-tree-sitter--get-all-sections
but may have subtle differences in usage."
  (let ((sections '())
        (child-count (treesit-node-child-count root-node)))
    (dotimes (i child-count)
      (let ((child (treesit-node-child root-node i)))
        (when (equal (treesit-node-type child) "section")
          (push child sections))))
    (nreverse sections)))

(defun greger-tree-sitter--extract-section (section-node)
  "Extract a dialog message from a tree-sitter SECTION-NODE.

INPUT:
  SECTION-NODE - A tree-sitter node representing a single conversation section
                 (e.g., ## USER:, ## ASSISTANT:, ## TOOL USE:, etc.)

PROCESSING:
  1. Determines the section type by examining the first child node
  2. Dispatches to the appropriate extraction function based on type:
     - user_section → user message with string content
     - assistant_section → assistant message with string content
     - thinking_section → assistant message with thinking content block
     - tool_use_section → assistant message with tool_use content block
     - tool_result_section → user message with tool_result content block
     - server_tool_use_section → assistant message with server_tool_use block
     - server_tool_result_section → assistant message with web_search_tool_result block
     - system_section → system message with string content

OUTPUT:
  Returns a message object with 'role and 'content fields, or nil if the
  section type is not recognized.

INTERNAL FUNCTION: Central dispatcher for section extraction."
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
     ((equal section-type "citations_with_text")
      (greger-tree-sitter--extract-citations-with-text-section section-node))
     ((equal section-type "citations_without_text")
      (greger-tree-sitter--extract-citations-without-text-section section-node))
     (t nil))))

(defun greger-tree-sitter--extract-user-section (section-node)
  "Extract user message from a ## USER: SECTION-NODE.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## USER: section

PROCESSING:
  1. Gets the user_section child node
  2. Finds the section_content within it
  3. Extracts plain text content, trimming whitespace

OUTPUT:
  Returns a message object:
  ((role . \"user\") (content . \"extracted text content\"))

  If no content is found, content will be an empty string.

EXAMPLE INPUT SECTION:
  ## USER:

  Hello, how are you?

EXAMPLE OUTPUT:
  ((role . \"user\") (content . \"Hello, how are you?\"))"
  (let* ((user-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type user-section-node "section_content")))

    `((role . "user")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-system-section (section-node)
  "Extract system message from a ## SYSTEM: SECTION-NODE.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## SYSTEM: section

PROCESSING:
  1. Gets the system_section child node
  2. Finds the section_content within it
  3. Extracts plain text content, trimming whitespace

OUTPUT:
  Returns a message object:
  ((role . \"system\") (content . \"extracted text content\"))

  If no content is found, content will be an empty string.

EXAMPLE INPUT SECTION:
  ## SYSTEM:

  You are a helpful assistant.

EXAMPLE OUTPUT:
  ((role . \"system\") (content . \"You are a helpful assistant.\"))"
  (let* ((system-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type system-section-node "section_content")))
    `((role . "system")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-assistant-section (section-node)
  "Extract assistant message from a ## ASSISTANT: SECTION-NODE.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## ASSISTANT: section

PROCESSING:
  1. Gets the assistant_section child node
  2. Finds the section_content within it
  3. Extracts plain text content, trimming whitespace

OUTPUT:
  Returns a message object:
  ((role . \"assistant\") (content . \"extracted text content\"))

  If no content is found, content will be an empty string.

  Note: This function extracts basic assistant content. In practice, assistant
  sections often get processed by greger-tree-sitter--process-sections-with-citations
  which may convert the string content to structured content blocks if the text
  contains <cite> tags or other special formatting.

EXAMPLE INPUT SECTION:
  ## ASSISTANT:

  I'm doing well, thank you for asking!

EXAMPLE OUTPUT:
  ((role . \"assistant\") (content . \"I'm doing well, thank you for asking!\"))"
  (let* ((assistant-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type assistant-section-node "section_content")))
    `((role . "assistant")
      (content . ,(if content-node
                      (greger-tree-sitter--extract-content content-node)
                    "")))))

(defun greger-tree-sitter--extract-thinking-section (section-node)
  "Extract thinking content from a ## THINKING: section and return as assistant message.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## THINKING: section

PROCESSING:
  1. Gets the thinking_section child node
  2. Finds the section_content within it
  3. Extracts text content and wraps it in a thinking content block

OUTPUT:
  Returns an assistant message object with a thinking content block:
  ((role . \"assistant\")
   (content . (((type . \"thinking\") (thinking . \"extracted text\")))))

  Thinking sections are treated as assistant internal thoughts, so they
  always produce assistant messages with structured content.

EXAMPLE INPUT SECTION:
  ## THINKING:

  I need to think about this carefully before responding.

EXAMPLE OUTPUT:
  ((role . \"assistant\")
   (content . (((type . \"thinking\")
                (thinking . \"I need to think about this carefully before responding.\")))))"
  (let* ((thinking-section-node (treesit-node-child section-node 0))
         (content-node (greger-tree-sitter--find-child-by-type thinking-section-node "section_content")))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . ,(if content-node
                                    (greger-tree-sitter--extract-content content-node)
                                  ""))))))))

(defun greger-tree-sitter--extract-tool-use-section (section-node)
  "Extract tool use from a ## TOOL USE: section and return as assistant message.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## TOOL USE: section

PROCESSING:
  1. Extracts tool name from \"Name: tool-name\" line
  2. Extracts tool ID from \"ID: tool-id\" line
  3. Extracts parameters from \"### param-name\" sections with <tool.id>content</tool.id> blocks
  4. Converts parameter names to symbols for the input alist

OUTPUT:
  Returns an assistant message object with a tool_use content block:
  ((role . \"assistant\")
   (content . (((type . \"tool_use\")
                (id . \"tool-id\")
                (name . \"tool-name\")
                (input . ((param1 . \"value1\") (param2 . \"value2\")))))))

EXAMPLE INPUT SECTION:
  ## TOOL USE:

  Name: read-file
  ID: toolu_123

  ### path

  <tool.toolu_123>
  hello.txt
  </tool.toolu_123>

EXAMPLE OUTPUT:
  ((role . \"assistant\")
   (content . (((type . \"tool_use\")
                (id . \"toolu_123\")
                (name . \"read-file\")
                (input . ((path . \"hello.txt\")))))))"
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
  "Extract the value from a tool parameter PARAM-NODE.

INPUT:
  PARAM-NODE - Tree-sitter node representing a tool parameter with structure:
    ### param_name
    <tool.tool_id>
    parameter value content
    </tool.tool_id>

PROCESSING:
  1. Finds the param_value child (the <tool.id>...</tool.id> block)
  2. Extracts the content field within that block
  3. Trims whitespace from the extracted text
  4. Converts to number if the value is numeric

OUTPUT:
  Returns the parameter value as a number if numeric, string otherwise,
  or empty string if no content is found.

EXAMPLE INPUT:
  ### path
  <tool.toolu_123>
  /path/to/file.txt
  </tool.toolu_123>

EXAMPLE OUTPUT:
  \"/path/to/file.txt\"

EXAMPLE INPUT (numeric):
  ### context-lines
  <tool.toolu_123>
  2
  </tool.toolu_123>

EXAMPLE OUTPUT:
  2

INTERNAL FUNCTION: Used by greger-tree-sitter--extract-tool-use-section
to extract individual parameter values."
  (let ((param-block (treesit-node-child-by-field-name param-node "param_value")))
    (if param-block
        (let ((content-node (treesit-node-child-by-field-name param-block "content")))
          (if content-node
              (let ((value (string-trim (treesit-node-text content-node))))
                ;; Try to convert to number if it looks numeric
                (if (string-match-p "^[0-9]+\\(?:\\.[0-9]+\\)?$" value)
                    (string-to-number value)
                  ;; Check for boolean values
                  (cond
                   ((string= value "true") t)
                   ((string= value "false") nil)
                   (t value))))
            ""))
      "")))

(defun greger-tree-sitter--extract-tool-result-section (section-node)
  "Extract tool result from a ## TOOL RESULT: section and return as user message.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## TOOL RESULT: section

PROCESSING:
  1. Extracts tool ID from \"ID: tool-id\" line
  2. Extracts result content from <tool.id>content</tool.id> block
  3. Creates a user message with tool_result content block

OUTPUT:
  Returns a user message object with a tool_result content block:
  ((role . \"user\")
   (content . (((type . \"tool_result\")
                (tool_use_id . \"tool-id\")
                (content . \"result content\")))))

  Tool results are considered user messages because they represent
  external system responses that the assistant receives.

EXAMPLE INPUT SECTION:
  ## TOOL RESULT:

  ID: toolu_123

  <tool.toolu_123>
  File contents: Hello, world!
  </tool.toolu_123>

EXAMPLE OUTPUT:
  ((role . \"user\")
   (content . (((type . \"tool_result\")
                (tool_use_id . \"toolu_123\")
                (content . \"File contents: Hello, world!\")))))"
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
  "Extract server tool use from a ## SERVER TOOL USE: section.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## SERVER TOOL USE: section

PROCESSING:
  1. Uses greger-tree-sitter--extract-tool-use-section to extract basic tool structure
  2. Changes the content block type from \"tool_use\" to \"server_tool_use\"

  Server tool use has the same structure as regular tool use but represents
  tools called on the server side rather than client side.

OUTPUT:
  Returns an assistant message object with a server_tool_use content block:
  ((role . \"assistant\")
   (content . (((type . \"server_tool_use\")
                (id . \"tool-id\")
                (name . \"tool-name\")
                (input . ((param . \"value\")))))))

EXAMPLE INPUT SECTION:
  ## SERVER TOOL USE:

  Name: web_search
  ID: srvtoolu_123

  ### query

  <tool.srvtoolu_123>
  search terms
  </tool.srvtoolu_123>

EXAMPLE OUTPUT:
  ((role . \"assistant\")
   (content . (((type . \"server_tool_use\")
                (id . \"srvtoolu_123\")
                (name . \"web_search\")
                (input . ((query . \"search terms\")))))))"
  ;; Similar to tool_use but with server_tool_use type
  (let ((result (greger-tree-sitter--extract-tool-use-section section-node)))
    ;; Change the type to server_tool_use
    (when result
      (let ((content (alist-get 'content result)))
        (when (and content (> (length content) 0))
          (setf (alist-get 'type (car content)) "server_tool_use"))))
    result))

(defun greger-tree-sitter--extract-citations-with-text-section (section-node)
  "Extract citations_with_text from SECTION-NODE and return as assistant message.

INPUT:
  SECTION-NODE - Tree-sitter node representing a citations_with_text section

PROCESSING:
  1. Extracts content_with_cites to get text with <cite> tags
  2. Extracts citations to get citation metadata
  3. Creates structured content blocks where cited text has citations attached

OUTPUT:
  Returns an assistant message object with structured content blocks:
  ((role . \"assistant\")
   (content . (((type . \"text\") (text . \"pre-cite text\"))
               ((type . \"text\") (text . \"cited text\") (citations . citations-list))
               ((type . \"text\") (text . \"post-cite text\")))))

EXAMPLE INPUT SECTION:
  ## ASSISTANT:

  Based on research, <cite>the sky is blue</cite> according to science.

  ## CITATIONS:

  ### https://example.com
  Title: Example
  Cited text: The sky is blue
  Encrypted index: abc123

EXAMPLE OUTPUT:
  ((role . \"assistant\")
   (content . (((type . \"text\") (text . \"Based on research,\"))
               ((type . \"text\") (text . \"the sky is blue\") (citations . citations-list))
               ((type . \"text\") (text . \"according to science.\")))))"
  (let* ((citations-with-text-node (treesit-node-child section-node 0))
         (content-node (treesit-node-child-by-field-name citations-with-text-node "content_with_cites"))
         (citations-node (treesit-node-child-by-field-name citations-with-text-node "citations")))

    ;; Extract citations first
    (let ((citations (if citations-node
                         (greger-tree-sitter--extract-citations-content citations-node)
                       '())))

      ;; Extract content and create citation-aware blocks
      (let ((content-blocks (if content-node
                                (greger-tree-sitter--extract-content-with-citations content-node citations)
                              '())))

        `((role . "assistant")
          (content . ,(if content-blocks content-blocks '())))))))

(defun greger-tree-sitter--extract-citations-without-text-section (section-node)
  "Extract citations_without_text from SECTION-NODE.

INPUT:
  SECTION-NODE - Tree-sitter node representing a citations_without_text section

PROCESSING:
  Extracts standalone citations that are not associated with any cite tags.
  This creates a special citations_without_text content block.

OUTPUT:
  Returns an assistant message with a citations_without_text content block:
  ((role . \"assistant\")
   (content . (((type . \"citations_without_text\") (entries . citations-list)))))

  Note: This is a special case that shouldn't typically appear in final output
  but represents standalone citation sections.

EXAMPLE INPUT SECTION:
  ## CITATIONS:

  ### https://example.com
  Title: Example
  Cited text: Some text
  Encrypted index: abc123

EXAMPLE OUTPUT:
  ((role . \"assistant\")
   (content . (((type . \"citations_without_text\")
                (entries . (((type . \"web_search_result_location\")
                             (url . \"https://example.com\")
                             (title . \"Example\")
                             (cited_text . \"Some text\")
                             (encrypted_index . \"abc123\"))))))))"
  (let* ((citations-without-text-node (treesit-node-child section-node 0))
         (citations-node (treesit-node-child-by-field-name citations-without-text-node "citations")))

    (let ((citations (if citations-node
                         (greger-tree-sitter--extract-citations-content citations-node)
                       '())))

      `((role . "assistant")
        (content . (((type . "citations_without_text") (entries . ,citations))))))))

(defun greger-tree-sitter--extract-server-tool-result-section (section-node)
  "Extract server tool result from a ## SERVER TOOL RESULT: section.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## SERVER TOOL RESULT: section

PROCESSING:
  1. Uses greger-tree-sitter--extract-tool-result-section to extract basic structure
  2. Changes role from \"user\" to \"assistant\" (server results are part of assistant flow)
  3. Changes content type from \"tool_result\" to \"web_search_tool_result\"
  4. Attempts to parse content as JSON, falling back to string if parsing fails

OUTPUT:
  Returns an assistant message object with a web_search_tool_result content block:
  ((role . \"assistant\")
   (content . (((type . \"web_search_tool_result\")
                (tool_use_id . \"tool-id\")
                (content . parsed-json-or-string)))))

  The content field will contain either:
  - Parsed JSON as an alist/list structure if content is valid JSON
  - Original string content if JSON parsing fails

EXAMPLE INPUT SECTION:
  ## SERVER TOOL RESULT:

  ID: srvtoolu_123

  <tool.srvtoolu_123>
  [{\"title\": \"Example\", \"url\": \"https://example.com\"}]
  </tool.srvtoolu_123>

EXAMPLE OUTPUT:
  ((role . \"assistant\")
   (content . (((type . \"web_search_tool_result\")
                (tool_use_id . \"srvtoolu_123\")
                (content . (((title . \"Example\") (url . \"https://example.com\"))))))))"
  (let ((result (greger-tree-sitter--extract-tool-result-section section-node)))
    ;; Change role to assistant and type to web_search_tool_result
    (when result
      (setf (alist-get 'role result) "assistant")
      (let ((content (alist-get 'content result)))
        (when (and content (> (length content) 0))
          (setf (alist-get 'type (car content)) "web_search_tool_result")
          ;; Try to parse JSON content
          (let ((content-text (alist-get 'content (car content))))
            (when (stringp content-text)
              (condition-case nil
                  (let ((parsed-json (json-parse-string content-text :object-type 'alist :array-type 'list)))
                    (setf (alist-get 'content (car content)) parsed-json))
                (error
                 ;; If JSON parsing fails, keep as string
                 nil)))))))
    result))

(defun greger-tree-sitter--find-child-by-type (node type)
  "Find the first child of NODE with the given TYPE.

INPUT:
  NODE - A tree-sitter node to search within
  TYPE - String representing the node type to find (e.g., \"section_content\")

PROCESSING:
  Iterates through all direct children of the node and returns the first
  one that matches the specified type.

OUTPUT:
  Returns the first matching child node, or nil if no match is found.

INTERNAL FUNCTION: Utility for navigating tree-sitter parse trees when
looking for specific types of child nodes."
  (let ((child-count (treesit-node-child-count node))
        (found nil))
    (dotimes (i child-count)
      (let ((child (treesit-node-child node i)))
        (when (and (not found) (equal (treesit-node-type child) type))
          (setq found child))))
    found))

(defun greger-tree-sitter--transform-sections-for-citations (sections)
  "Transform SECTIONS according to citation requirements.

INPUT:
  SECTIONS - List of raw tree-sitter section nodes

PROCESSING:
  Implements your exact requirements:
  1. If <cite>...</cite> is encountered anywhere on a line, starts parsing a new citations_with_text object
  2. Content inside <cite>...</cite> becomes 'text field on citations_with_text object
  3. After </cite>, assumes whitespace until ## CITATIONS: at start of new line
  4. Parses those citations and places each entry as 'entry field on citations_with_text object
  5. If ## CITATIONS: is encountered when not parsing citations_with_text, makes citations_without_text object

OUTPUT:
  Returns list of transformed section objects that include citations_with_text and citations_without_text."
  (let ((result '())
        (i 0))

    (while (< i (length sections))
      (let* ((section (nth i sections))
             (section-type (greger-tree-sitter--get-section-type section)))

        ;; Check if this section contains cite tags
        (if (greger-tree-sitter--section-contains-cite-tags section)
            ;; Found cite tags - look for following citations section
            (let ((citations-section-idx (greger-tree-sitter--find-next-citations-section sections (1+ i))))
              (if citations-section-idx
                  ;; Found citations section - create citations_with_text
                  (let* ((citations-section (nth citations-section-idx sections))
                         (citations-with-text-obj (greger-tree-sitter--create-citations-with-text-section
                                                   section citations-section)))
                    (push citations-with-text-obj result)
                    ;; Skip the citations section since we consumed it
                    (setq i citations-section-idx))
                ;; No citations section found - treat as regular section
                (push section result)))

          ;; Check if this is a standalone citations section
          (if (equal section-type "citations_section")
              ;; Check if it was already consumed by a citations_with_text
              (let ((was-consumed (and (> i 0)
                                       (greger-tree-sitter--section-contains-cite-tags (nth (1- i) sections)))))
                (unless was-consumed
                  ;; Standalone citations section - create citations_without_text
                  (let ((citations-without-text-obj (greger-tree-sitter--create-citations-without-text-section section)))
                    (push citations-without-text-obj result))))
            ;; Regular section - pass through
            (push section result))))

      (setq i (1+ i)))

    (nreverse result)))

(defun greger-tree-sitter--section-contains-cite-tags (section)
  "Check if SECTION contains <cite> tags anywhere on a line."
  (let ((section-type (greger-tree-sitter--get-section-type section)))
    (when (member section-type '("assistant_section" "thinking_section" "user_section" "system_section"))
      (let ((content (greger-tree-sitter--get-raw-section-content section)))
        (and content (string-match-p "<cite>" content))))))

(defun greger-tree-sitter--get-raw-section-content (section)
  "Get raw text content from SECTION node."
  (let* ((section-child (treesit-node-child section 0))
         (content-node (greger-tree-sitter--find-child-by-type section-child "section_content")))
    (when content-node
      (treesit-node-text content-node))))

(defun greger-tree-sitter--create-citations-with-text-section (content-section citations-section)
  "Create a citations_with_text section object from CONTENT-SECTION and CITATIONS-SECTION."
  (let* ((content-text (greger-tree-sitter--get-raw-section-content content-section))
         (citations (greger-tree-sitter--extract-citations-section citations-section))
         (cite-texts (greger-tree-sitter--extract-cite-texts content-text)))
    `((type . "citations_with_text")
      (original-section . ,content-section)
      (texts . ,cite-texts)
      (entries . ,citations))))

(defun greger-tree-sitter--extract-cite-texts (content)
  "Extract all <cite>text</cite> content from CONTENT string."
  (let ((texts '())
        (start 0))
    (while (string-match "<cite>\\(.*?\\)</cite>" content start)
      (push (match-string 1 content) texts)
      (setq start (match-end 0)))
    (nreverse texts)))

(defun greger-tree-sitter--create-citations-without-text-section (citations-section)
  "Create a citations_without_text section object from CITATIONS-SECTION."
  (let ((citations (greger-tree-sitter--extract-citations-section citations-section)))
    `((type . "citations_without_text")
     (original-section . ,citations-section)
     (entries . ,citations))))

(defun greger-tree-sitter--section-was-transformed (section transformed-sections)
  "Check if SECTION was transformed and should be skipped in regular processing."
  (let ((found nil))
    (dolist (transformed transformed-sections)
      (when (and (listp transformed)
                 (alist-get 'original-section transformed)
                 (eq section (alist-get 'original-section transformed)))
        (setq found t)))
    found))

(defun greger-tree-sitter--process-transformed-sections (transformed-sections)
  "Process TRANSFORMED-SECTIONS that may include citations_with_text and citations_without_text."
  (let ((messages '())
        (current-assistant-blocks '()))

    (dolist (section transformed-sections)
      (let ((section-type (if (listp section)
                              (alist-get 'type section)
                            (greger-tree-sitter--get-section-type section))))

        (cond
         ((equal section-type "citations_with_text")
          ;; Process citations_with_text
          (let ((texts (alist-get 'texts section))
                (entries (alist-get 'entries section))
                (original-section (alist-get 'original-section section)))
            ;; Extract any non-cite content from original section
            (let ((blocks (greger-tree-sitter--extract-blocks-from-citations-with-text original-section texts entries)))
              (setq current-assistant-blocks (append current-assistant-blocks blocks)))))

         ((equal section-type "citations_without_text")
          ;; Process citations_without_text
          (let ((entries (alist-get 'entries section)))
            (setq current-assistant-blocks
                  (append current-assistant-blocks
                          `(((type . "citations_without_text") (entries . ,entries)))))))

         ;; Handle regular sections (existing logic)
         (t
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message
              (let ((role (alist-get 'role message))
                    (content (alist-get 'content message)))

                (cond
                 ;; User/System sections - flush assistant blocks
                 ((member role '("user" "system"))
                  (when current-assistant-blocks
                    (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
                      (push `((role . "assistant") (content . ,assistant-content)) messages))
                    (setq current-assistant-blocks '()))
                  (push message messages))

                 ;; Assistant sections - accumulate blocks
                 ((equal role "assistant")
                  (if (listp content)
                      (setq current-assistant-blocks (append current-assistant-blocks content))
                    (when (and (stringp content) (> (length (string-trim content)) 0))
                      (setq current-assistant-blocks (append current-assistant-blocks `(((type . "text") (text . ,content))))))))))))))))

    ;; Flush any remaining assistant blocks
    (when current-assistant-blocks
      (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
        (push `((role . "assistant") (content . ,assistant-content)) messages)))

    (nreverse messages)))

(defun greger-tree-sitter--extract-blocks-from-citations-with-text (section cite-texts entries)
  "Extract content blocks from SECTION, creating citations_with_text blocks for CITE-TEXTS with ENTRIES."
  (let* ((content (greger-tree-sitter--get-raw-section-content section))
         (blocks '())
         (pos 0))

    ;; Process content, splitting by cite tags
    (dolist (cite-text cite-texts)
      (let ((cite-pattern (concat "<cite>" (regexp-quote cite-text) "</cite>")))
        (when (string-match cite-pattern content pos)
          ;; Add any text before the cite tag
          (let ((before-text (substring content pos (match-beginning 0))))
            (when (> (length (string-trim before-text)) 0)
              (push `((type . "text") (text . ,(string-trim before-text))) blocks)))

          ;; Add the citations_with_text block
          (push `((type . "text") (text . ,cite-text) (citations . ,entries)) blocks)

          (setq pos (match-end 0)))))

    ;; Add any remaining text after last cite tag
    (when (< pos (length content))
      (let ((remaining-text (substring content pos)))
        (when (> (length (string-trim remaining-text)) 0)
          (push `((type . "text") (text . ,(string-trim remaining-text))) blocks))))

    (nreverse blocks)))

(defun greger-tree-sitter--get-all-sections (root-node)
  "Extract all section nodes from ROOT-NODE in document order.

INPUT:
  ROOT-NODE - Tree-sitter root node (typically a \"source_file\" node)

PROCESSING:
  Iterates through all direct children of the root node and collects
  those with type \"section\". Maintains document order.

OUTPUT:
  Returns a list of section nodes in the order they appear in the document.
  Each section corresponds to a ## HEADER: block in the greger format.

INTERNAL FUNCTION: Similar to greger-tree-sitter--get-sections but used
in the main parsing flow for multi-section documents."
  (let ((sections '())
        (child-count (treesit-node-child-count root-node)))
    (dotimes (i child-count)
      (let ((child (treesit-node-child root-node i)))
        (when (equal (treesit-node-type child) "section")
          (push child sections))))
    (nreverse sections)))

(defun greger-tree-sitter--process-sections-with-citations (sections)
  "Process conversation SECTIONS and handle citation associations.

INPUT:
  SECTIONS - List of tree-sitter section nodes representing the parsed conversation

PROCESSING:
  This is the core function that handles the complex citation workflow:

  1. Iterates through sections in order
  2. Groups assistant-related sections together (assistant, thinking, tool_use, etc.)
  3. When a ## CITATIONS: section is encountered, associates those citations
     with any <cite> tags in the pending assistant content blocks
  4. Flushes assistant blocks when non-assistant sections are encountered
  5. Reorders final assistant blocks to put tools first, then text

  Section handling:
  - user_section, system_section → flush assistant blocks, add as separate message
  - assistant_section, thinking_section, tool_use_section, server_tool_use_section,
    server_tool_result_section → accumulate as assistant content blocks
  - citations_section → associate with pending assistant blocks containing <cite> tags
  - tool_result_section → flush assistant blocks, add as user message

OUTPUT:
  Returns a list of complete message objects where:
  - Each message has role and content fields
  - Assistant messages have structured content blocks
  - Citations are properly linked to cited text blocks
  - Tool sequences are properly ordered

INTERNAL FUNCTION: This implements the complex citation parsing logic that
makes <cite>text</cite> tags work with subsequent ## CITATIONS: sections."
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
            (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
              (push `((role . "assistant") (content . ,assistant-content)) messages))
            (setq current-assistant-blocks '()))
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message (push message messages))))

         ;; System section - flush any pending assistant blocks and add system message
         ((equal section-type "system_section")
          (when current-assistant-blocks
            (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
              (push `((role . "assistant") (content . ,assistant-content)) messages))
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
                  ;; Check for cite tags and look ahead for citations section
                  (when (and (stringp content) (> (length (string-trim content)) 0))
                    (if (string-match-p "<cite>" content)
                        ;; Content has cite tags - look for following citations section
                        (let ((next-citations-idx (greger-tree-sitter--find-next-citations-section sections (1+ i))))
                          (if next-citations-idx
                              ;; Found citations section - create citations_with_text blocks
                              (let* ((citations-section (nth next-citations-idx sections))
                                     (citations (greger-tree-sitter--extract-citations-section citations-section))
                                     (cite-blocks (greger-tree-sitter--create-citations-with-text-blocks content citations)))
                                (setq current-assistant-blocks (append current-assistant-blocks cite-blocks))
                                ;; Mark this citations section as processed by skipping to it
                                (setq i next-citations-idx))
                            ;; No citations section found - treat as regular text
                            (setq current-assistant-blocks (append current-assistant-blocks `(((type . "text") (text . ,content)))))))
                      ;; Regular text without cite tags
                      (setq current-assistant-blocks (append current-assistant-blocks `(((type . "text") (text . ,content))))))))))))

         ;; Citations section - handle as citations_without_text if not already processed
         ((equal section-type "citations_section")
          ;; Check if this citations section follows a section with cite tags
          (let ((has-preceding-cites (and (> i 0)
                                          (greger-tree-sitter--section-has-cite-tags (nth (1- i) sections)))))
            (unless has-preceding-cites
              ;; This is a standalone citations section - create citations_without_text
              (let ((citations (greger-tree-sitter--extract-citations-section section)))
                (setq current-assistant-blocks
                      (append current-assistant-blocks
                              `(((type . "citations_without_text") (entries . ,citations)))))))))

         ;; Tool result section - add as user message
         ((equal section-type "tool_result_section")
          (when current-assistant-blocks
            (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
              (push `((role . "assistant") (content . ,assistant-content)) messages))
            (setq current-assistant-blocks '()))
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message (push message messages))))))

      (setq i (1+ i)))

    ;; Flush any remaining assistant blocks
    (when current-assistant-blocks
      (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
        (push `((role . "assistant") (content . ,assistant-content)) messages)))

    (nreverse messages)))

(defun greger-tree-sitter--find-next-citations-section (sections start-idx)
  "Find the next citations section starting from START-IDX in SECTIONS.

Returns the index of the citations section, or nil if not found."
  (let ((i start-idx)
        (found nil))
    (while (and (< i (length sections)) (not found))
      (let ((section-type (greger-tree-sitter--get-section-type (nth i sections))))
        (if (equal section-type "citations_section")
            (setq found i)
          (setq i (1+ i)))))
    found))

(defun greger-tree-sitter--section-has-cite-tags (section)
  "Check if SECTION contains cite tags in its content."
  (let ((section-type (greger-tree-sitter--get-section-type section)))
    (when (member section-type '("assistant_section" "thinking_section"))
      (let ((message (greger-tree-sitter--extract-section section)))
        (when message
          (let ((content (alist-get 'content message)))
            (and (stringp content) (string-match-p "<cite>" content))))))))

(defun greger-tree-sitter--extract-citations-with-text-blocks (content)
  "Extract content blocks from CONTENT that contains cite tags.

Creates structured blocks according to citations_with_text format."
  (greger-tree-sitter--parse-content-with-citations content nil))

(defun greger-tree-sitter--create-citations-with-text-blocks (content citations)
  "Create citations_with_text blocks from CONTENT with cite tags and CITATIONS.

This implements your requirement: when <cite>...</cite> is encountered,
create a citations_with_text object with 'text field containing the cited text
and 'entries field containing the citation entries."
  (let ((parts '())
        (current-pos 0))

    ;; Find all cite tags and split content accordingly
    (while (string-match "<cite>\\(.*?\\)</cite>" content current-pos)
      (let ((before-cite (substring content current-pos (match-beginning 0)))
            (cite-text (match-string 1 content))
            (after-match (match-end 0)))

        ;; Add text before cite if any (as regular text block)
        (when (> (length (string-trim before-cite)) 0)
          (push `((type . "text") (text . ,(string-trim before-cite))) parts))

        ;; Add citations_with_text block
        (push `((type . "citations_with_text")
                (text . ,cite-text)
                (entries . ,citations)) parts)

        (setq current-pos after-match)))

    ;; Add remaining text after last cite (as regular text block)
    (let ((remaining (substring content current-pos)))
      (when (> (length (string-trim remaining)) 0)
        (push `((type . "text") (text . ,(string-trim remaining))) parts)))

    (nreverse parts)))

(defun greger-tree-sitter--get-section-type (section-node)
  "Get the type of a SECTION-NODE.

INPUT:
  SECTION-NODE - A tree-sitter section node

PROCESSING:
  Gets the type of the first child node, which indicates what kind of
  section this is (user_section, assistant_section, citations_section, etc.)

OUTPUT:
  Returns a string representing the section type:
  - \"user_section\" for ## USER: sections
  - \"assistant_section\" for ## ASSISTANT: sections
  - \"system_section\" for ## SYSTEM: sections
  - \"thinking_section\" for ## THINKING: sections
  - \"tool_use_section\" for ## TOOL USE: sections
  - \"tool_result_section\" for ## TOOL RESULT: sections
  - \"server_tool_use_section\" for ## SERVER TOOL USE: sections
  - \"server_tool_result_section\" for ## SERVER TOOL RESULT: sections
  - \"citations_section\" for ## CITATIONS: sections

INTERNAL FUNCTION: Used by section processing logic to dispatch to
appropriate extraction functions."
  (treesit-node-type (treesit-node-child section-node 0)))

(defun greger-tree-sitter--associate-citations-with-blocks (content-blocks citations)
  "Associate CITATIONS with content blocks that contain <cite> tags.

INPUT:
  CONTENT-BLOCKS - List of content block objects (from assistant message)
  CITATIONS - List of citation objects extracted from ## CITATIONS: section

PROCESSING:
  1. Iterates through content blocks
  2. For text blocks containing <cite> tags:
     - Parses the text to split by citation boundaries
     - Associates citations with the cited text portions
     - Replaces the single block with multiple parsed blocks
  3. For other blocks (no cite tags):
     - Keeps them unchanged

OUTPUT:
  Returns a new list of content blocks where:
  - Text blocks with <cite> tags are split into multiple text blocks
  - Cited text blocks have citations attached
  - Non-cited text blocks and other block types remain unchanged

EXAMPLE INPUT:
  CONTENT-BLOCKS: (((type . \"text\") (text . \"Hello <cite>world</cite>!\")))
  CITATIONS: (((url . \"http://example.com\") ...))

EXAMPLE OUTPUT:
  (((type . \"text\") (text . \"Hello\"))
   ((type . \"text\") (text . \"world\") (citations . citations-list))
   ((type . \"text\") (text . \"!\")))

INTERNAL FUNCTION: Core of the citation association logic that makes
<cite> tags work with subsequent ## CITATIONS: sections."
  (let ((result '()))
    (dolist (block content-blocks)
      (if (and (equal (alist-get 'type block) "text")
               (string-match-p "<cite>" (alist-get 'text block "")))
          ;; This text block has cite tags - parse it with citations
          (let ((parsed-blocks (greger-tree-sitter--parse-content-with-citations
                                (alist-get 'text block) citations)))
            (setq result (append result parsed-blocks)))
        ;; Regular block - add as is
        (setq result (append result (list block)))))
    result))

(defun greger-tree-sitter--extract-content-with-citations (content-node citations)
  "Extract content blocks from CONTENT-NODE that contains cite tags, associating CITATIONS.

INPUT:
  CONTENT-NODE - Tree-sitter node representing content_with_cites
  CITATIONS - List of citation objects to associate with cited text

PROCESSING:
  1. Iterates through content_line_with_cites nodes
  2. For each line, processes text and cite_tag nodes
  3. Creates separate text blocks for regular text and cited text
  4. Associates citations with cited text blocks

OUTPUT:
  Returns a list of content block objects:
  (((type . \"text\") (text . \"regular text\"))
   ((type . \"text\") (text . \"cited text\") (citations . citations-list))
   ((type . \"text\") (text . \"more regular text\")))

INTERNAL FUNCTION: Used by greger-tree-sitter--extract-citations-with-text-section
to process content that contains <cite> tags."
  (let ((blocks '())
        (child-count (treesit-node-child-count content-node)))

    (dotimes (i child-count)
      (let* ((child (treesit-node-child content-node i))
             (child-type (treesit-node-type child)))

        (when (equal child-type "content_line_with_cites")
          ;; Process this line for text and cite tags
          (let ((line-blocks (greger-tree-sitter--extract-line-with-citations child citations)))
            (setq blocks (append blocks line-blocks))))))

    blocks))

(defun greger-tree-sitter--extract-line-with-citations (line-node citations)
  "Extract content blocks from a single LINE-NODE that may contain cite tags.

INPUT:
  LINE-NODE - Tree-sitter node representing a content_line_with_cites
  CITATIONS - List of citation objects to associate

PROCESSING:
  1. Iterates through children of the line (text and cite_tag nodes)
  2. Creates text blocks for regular text
  3. Creates text blocks with citations for cite_tag nodes
  4. Maintains order of text and cited content

OUTPUT:
  Returns a list of content blocks for this line.

INTERNAL FUNCTION: Used by greger-tree-sitter--extract-content-with-citations."
  (let ((line-blocks '())
        (child-count (treesit-node-child-count line-node)))

    (dotimes (i child-count)
      (let* ((child (treesit-node-child line-node i))
             (child-type (treesit-node-type child)))

        (cond
         ((equal child-type "text")
          ;; Regular text
          (let ((text (string-trim (treesit-node-text child))))
            (when (> (length text) 0)
              (push `((type . "text") (text . ,text)) line-blocks))))

         ((equal child-type "cite_tag")
          ;; Cited text
          (let ((cited-text-node (treesit-node-child-by-field-name child "cited_text")))
            (when cited-text-node
              (let ((cited-text (string-trim (treesit-node-text cited-text-node))))
                (when (> (length cited-text) 0)
                  (push `((type . "text") (text . ,cited-text) (citations . ,citations)) line-blocks)))))))))

    (nreverse line-blocks)))

(defun greger-tree-sitter--extract-citations-content (citations-node)
  "Extract citations from a citations_content CITATIONS-NODE.

INPUT:
  CITATIONS-NODE - Tree-sitter node representing citations_content

PROCESSING:
  1. Iterates through citation_entry children
  2. Extracts each citation using greger-tree-sitter--extract-citation-entry

OUTPUT:
  Returns a list of citation objects.

INTERNAL FUNCTION: Used by citation extraction functions to process
the citations_content portion of citation sections."
  (let ((citations '())
        (child-count (treesit-node-child-count citations-node)))

    (dotimes (i child-count)
      (let* ((child (treesit-node-child citations-node i))
             (child-type (treesit-node-type child)))

        (when (equal child-type "citation_entry")
          (let ((citation (greger-tree-sitter--extract-citation-entry child)))
            (when citation
              (push citation citations))))))

    (nreverse citations)))

(defun greger-tree-sitter--extract-citations-section (section-node)
  "Extract citations from a ## CITATIONS: SECTION-NODE.

INPUT:
  SECTION-NODE - Tree-sitter node representing a ## CITATIONS: section

PROCESSING:
  1. Finds the citations_content within the section
  2. Iterates through all citation_entry children
  3. Extracts each citation entry using greger-tree-sitter--extract-citation-entry

OUTPUT:
  Returns a list of citation objects, each with fields:
  - type: \"web_search_result_location\"
  - url: The URL from ### https://... line
  - title: Text after \"Title:\" line
  - cited_text: Text after \"Cited text:\" line
  - encrypted_index: Text after \"Encrypted index:\" line

EXAMPLE INPUT SECTION:
  ## CITATIONS:

  ### https://example.com

  Title: Example Site
  Cited text: This is the relevant text from the source
  Encrypted index: abc123def

EXAMPLE OUTPUT:
  (((type . \"web_search_result_location\")
    (url . \"https://example.com\")
    (title . \"Example Site\")
    (cited_text . \"This is the relevant text from the source\")
    (encrypted_index . \"abc123def\")))

INTERNAL FUNCTION: Used by citation association logic to extract citation
metadata that gets linked to <cite> tags."
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
  "Extract a single citation entry from ENTRY-NODE.

INPUT:
  ENTRY-NODE - Tree-sitter node representing one citation entry (### URL and its fields)

PROCESSING:
  1. Extracts URL from the field name (### https://... line)
  2. Iterates through child nodes to find:
     - citation_title nodes → extracts title after \"Title:\"
     - citation_text nodes → extracts text after \"Cited text:\"
     - citation_index nodes → extracts index after \"Encrypted index:\"
  3. Uses field names when available, falls back to regex extraction
  4. Trims whitespace from all extracted values

OUTPUT:
  Returns a citation object:
  ((type . \"web_search_result_location\")
   (url . \"extracted-url\")
   (title . \"extracted-title\")
   (cited_text . \"extracted-cited-text\")
   (encrypted_index . \"extracted-index\"))

  All fields will be present but may be nil/empty if not found in the input.

EXAMPLE INPUT:
  ### https://example.com

  Title: Example Website
  Cited text: Relevant quote from the website
  Encrypted index: abc123

EXAMPLE OUTPUT:
  ((type . \"web_search_result_location\")
   (url . \"https://example.com\")
   (title . \"Example Website\")
   (cited_text . \"Relevant quote from the website\")
   (encrypted_index . \"abc123\"))

INTERNAL FUNCTION: Used by greger-tree-sitter--extract-citations-section
to process individual citation entries."
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
                      )
                  ;; Fallback: try to extract from the text after "Cited text:"
                  (let ((text (treesit-node-text child)))
                    (when (string-match "Cited text:[ \t]*\\(.*\\)" text)
                      (setq cited-text (string-trim (match-string 1 text)))
                      )))))
             ((equal (treesit-node-type child) "citation_index")
              (let ((index-node (treesit-node-child-by-field-name child "index")))

                (if index-node
                    (progn
                      (setq encrypted-index (string-trim (treesit-node-text index-node)))
                      )
                  ;; Fallback: try to extract from the text after "Encrypted index:"
                  (let ((text (treesit-node-text child)))
                    (when (string-match "Encrypted index:[ \t]*\\(.*\\)" text)
                      (setq encrypted-index (string-trim (match-string 1 text)))
                      )))))))))


      `((type . "web_search_result_location")
        (url . ,url)
        (title . ,title)
        (cited_text . ,cited-text)
        (encrypted_index . ,encrypted-index)))))

(defun greger-tree-sitter--message-has-cite-tags (message)
  "Check if MESSAGE content contains <cite> tags.

INPUT:
  MESSAGE - A message object with 'content field

PROCESSING:
  Checks if the content field is a string and contains \"<cite>\" substring.

OUTPUT:
  Returns t if the message content contains cite tags, nil otherwise.

  Note: This function only works with string content, not structured
  content blocks. It's used for simple cite tag detection.

INTERNAL FUNCTION: Utility for determining if a message needs citation
processing. May be used in older/simpler citation workflows."
  (let ((content (alist-get 'content message)))
    (and (stringp content)
         (string-match-p "<cite>" content))))

(defun greger-tree-sitter--associate-citations (message citations)
  "Associate CITATIONS with cited text in MESSAGE (legacy function).

INPUT:
  MESSAGE - A message object with string content containing <cite> tags
  CITATIONS - List of citation objects to associate

PROCESSING:
  1. Checks if content is a string
  2. If so, parses it with greger-tree-sitter--parse-content-with-citations
  3. Replaces the string content with structured content blocks

OUTPUT:
  Returns the modified message with structured content blocks instead
  of string content.

LEGACY FUNCTION: This function provides simple citation association for
messages with string content. The main citation processing now happens
in greger-tree-sitter--process-sections-with-citations which handles
more complex cases with mixed content blocks."
  ;; This is complex - we need to parse the content and split it by cite tags
  (let ((content (alist-get 'content message)))
    (when (stringp content)
      (setf (alist-get 'content message)
            (greger-tree-sitter--parse-content-with-citations content citations))))
  message)

(defun greger-tree-sitter--parse-content-with-citations (content citations)
  "Parse CONTENT string and create content blocks with CITATIONS attached to cited text.

INPUT:
  CONTENT - String containing text with <cite>...</cite> tags
  CITATIONS - List of citation objects to associate with cited text

PROCESSING:
  1. Scans content for <cite>text</cite> patterns using regex matching
  2. Splits content into separate text blocks:
     - Text before citations (if any) → text block without citations
     - Text inside <cite> tags → text block with citations attached
     - Text after citations (if any) → text block without citations
  3. Removes the <cite> tags themselves, keeping only the inner text
  4. Trims whitespace from all text blocks

OUTPUT:
  Returns a list of content block objects:
  (((type . \"text\") (text . \"text before cite\"))
   ((type . \"text\") (text . \"cited text\") (citations . citations-list))
   ((type . \"text\") (text . \"text after cite\")))

  Empty text blocks are filtered out.

EXAMPLE INPUT:
  CONTENT: \"Based on research, <cite>the sky is blue</cite> according to science.\"
  CITATIONS: (((type . \"web_search_result_location\") (url . \"http://example.com\") ...))

EXAMPLE OUTPUT:
  (((type . \"text\") (text . \"Based on research,\"))
   ((type . \"text\") (text . \"the sky is blue\") (citations . citations-list))
   ((type . \"text\") (text . \"according to science.\")))

INTERNAL FUNCTION: Core citation parsing logic used by citation association functions."
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
  "Extract plain text content from CONTENT-NODE.

INPUT:
  CONTENT-NODE - Tree-sitter node representing section content

PROCESSING:
  Extracts text from content_line nodes, joining multiple lines with spaces.
  Each content_line contains text tokens that need to be concatenated.

OUTPUT:
  Returns the trimmed text content as a string, or empty string if the
  node is nil.

INTERNAL FUNCTION: Basic utility for extracting text content from
tree-sitter nodes. Used by all section extraction functions to get
the actual text content within sections."
  (if (not content-node)
      ""
    (let ((text-parts '())
          (child-count (treesit-node-child-count content-node)))
      (dotimes (i child-count)
        (let* ((child (treesit-node-child content-node i))
               (child-type (treesit-node-type child)))
          (when (equal child-type "content_line")
            ;; Try to extract text from content_line children first
            (let ((line-child-count (treesit-node-child-count child))
                  (line-texts '()))
              (dotimes (j line-child-count)
                (let* ((line-child (treesit-node-child child j))
                       (line-child-type (treesit-node-type line-child)))
                  (when (equal line-child-type "text")
                    (push (treesit-node-text line-child) line-texts))))
              ;; If no text children found, extract from the content_line directly
              (if line-texts
                  (push (string-join (nreverse line-texts) "") text-parts)
                (let ((line-text (treesit-node-text child)))
                  ;; Remove trailing newline and check if there's content
                  (when (and line-text (> (length line-text) 0))
                    (setq line-text (string-trim-right line-text "\n"))
                    (when (> (length line-text) 0)
                      (push line-text text-parts)))))))))
      (if text-parts
          (string-trim (string-join (nreverse text-parts) "\n"))
        ""))))

;; Test function
(defun greger-tree-sitter-test ()
  "Test the tree-sitter parser with a simple example.

This interactive function demonstrates basic usage of the greger tree-sitter
parser by parsing a simple conversation and displaying the result.

USAGE:
  M-x greger-tree-sitter-test

OUTPUT:
  Displays the parsed message structure in the *Messages* buffer.

TEST CASE:
  Parses a basic USER/ASSISTANT conversation to verify the parser is
  working correctly."
  (interactive)
  (let ((test-text "## USER:

Hello, how are you?

## ASSISTANT:

Hi there! How can I help you today?"))
    (message "Parsed result: %S" (greger-tree-sitter-parse test-text))))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
