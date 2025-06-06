;;; greger-tree-sitter.el --- Tree-sitter integration for greger format -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides tree-sitter parsing for the greger conversation format.
;; It parses markdown-like conversation logs with sections like ## USER:, ## ASSISTANT:,
;; ## TOOL USE:, etc., and converts them to structured dialog messages.
;;
;; The main entry point is `greger-tree-sitter-parse' which takes a markdown string
;; and returns a list of message objects compatible with greger.el's format.
;;
;; Key features:
;; - Parses user, assistant, system, thinking sections
;; - Handles tool use and tool result sections
;; - Supports server tool use and web search tool results
;; - Parses citations (<cite>...</cite> tags with ## CITATIONS: sections)
;; - Converts content to structured format with content blocks

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
  3. For multiple sections, processes them with citation handling
  4. For single section, extracts just that section

OUTPUT:
  Returns a list of message objects in the same format as greger-tree-sitter-parse.
  Messages are returned in the order they appear in the input text.

INTERNAL FUNCTION: This is called by greger-tree-sitter-parse and not intended
for direct use."
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

OUTPUT:
  Returns the parameter value as a trimmed string, or empty string if
  no content is found.

EXAMPLE INPUT:
  ### path
  <tool.toolu_123>
  /path/to/file.txt
  </tool.toolu_123>

EXAMPLE OUTPUT:
  \"/path/to/file.txt\"

INTERNAL FUNCTION: Used by greger-tree-sitter--extract-tool-use-section
to extract individual parameter values."
  (let ((param-block (treesit-node-child-by-field-name param-node "param_value")))
    (if param-block
        (let ((content-node (treesit-node-child-by-field-name param-block "content")))
          (if content-node
              (string-trim (treesit-node-text content-node))
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
      ;; Reorder assistant blocks to put server_tool_use first, then web_search_tool_result, then text blocks
      (let ((reordered-blocks (greger-tree-sitter--reorder-assistant-blocks (nreverse current-assistant-blocks))))
        (push `((role . "assistant") (content . ,reordered-blocks)) messages)))

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
