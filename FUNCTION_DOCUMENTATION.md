# greger-tree-sitter.el Function Documentation

This document provides a comprehensive overview of all functions in `greger-tree-sitter.el`, their inputs, outputs, and behavior.

## Public API

### `greger-tree-sitter-parse (text)`
**Main entry point** - Parse greger conversation text using tree-sitter.

**Input:** String containing greger conversation format (## USER:, ## ASSISTANT:, etc.)

**Output:** List of message objects compatible with greger.el format:
- `((role . "user") (content . "string"))`
- `((role . "assistant") (content . (content-block-list)))`

**Example:**
```elisp
(greger-tree-sitter-parse "## USER:\n\nHello\n\n## ASSISTANT:\n\nHi there!")
;; => (((role . "user") (content . "Hello"))
;;     ((role . "assistant") (content . "Hi there!")))
```

### `greger-tree-sitter-test ()`
**Interactive test function** - Demonstrates basic parser usage with a simple conversation.

## Core Processing Functions

### `greger-tree-sitter--extract-dialog (parser text)`
**Internal:** Extract dialog messages from parsed tree-sitter output.

**Input:** Tree-sitter parser object and original text string

**Output:** List of message objects

**Behavior:** Handles both single sections and multi-section documents, dispatches to citation processing for complex conversations.

### `greger-tree-sitter--process-sections-with-citations (sections)`
**Core citation processor** - Handles the complex citation workflow.

**Input:** List of tree-sitter section nodes

**Output:** List of complete message objects with citations properly linked

**Key Features:**
- Groups assistant-related sections together
- Associates `<cite>` tags with subsequent `## CITATIONS:` sections
- Reorders content blocks (tools → results → text)
- Handles multiple assistant sections

## Section Extraction Functions

Each section type has a dedicated extraction function:

### `greger-tree-sitter--extract-user-section (section-node)`
**Input:** `## USER:` section node
**Output:** `((role . "user") (content . "extracted text"))`

### `greger-tree-sitter--extract-assistant-section (section-node)`
**Input:** `## ASSISTANT:` section node
**Output:** `((role . "assistant") (content . "extracted text"))`

### `greger-tree-sitter--extract-system-section (section-node)`
**Input:** `## SYSTEM:` section node
**Output:** `((role . "system") (content . "extracted text"))`

### `greger-tree-sitter--extract-thinking-section (section-node)`
**Input:** `## THINKING:` section node
**Output:** Assistant message with thinking content block:
```elisp
((role . "assistant")
 (content . (((type . "thinking") (thinking . "extracted text")))))
```

### `greger-tree-sitter--extract-tool-use-section (section-node)`
**Input:** `## TOOL USE:` section node
**Output:** Assistant message with tool_use content block:
```elisp
((role . "assistant")
 (content . (((type . "tool_use")
              (id . "tool-id")
              (name . "tool-name")
              (input . ((param1 . "value1") (param2 . "value2")))))))
```

### `greger-tree-sitter--extract-tool-result-section (section-node)`
**Input:** `## TOOL RESULT:` section node
**Output:** User message with tool_result content block:
```elisp
((role . "user")
 (content . (((type . "tool_result")
              (tool_use_id . "tool-id")
              (content . "result content")))))
```

### `greger-tree-sitter--extract-server-tool-use-section (section-node)`
**Input:** `## SERVER TOOL USE:` section node
**Output:** Assistant message with server_tool_use content block (same structure as tool_use but different type)

### `greger-tree-sitter--extract-server-tool-result-section (section-node)`
**Input:** `## SERVER TOOL RESULT:` section node
**Output:** Assistant message with web_search_tool_result content block
**Special:** Attempts to parse content as JSON, falls back to string

## Citation Processing Functions

### `greger-tree-sitter--extract-citations-section (section-node)`
**Input:** `## CITATIONS:` section node
**Output:** List of citation objects:
```elisp
(((type . "web_search_result_location")
  (url . "https://example.com")
  (title . "Page Title")
  (cited_text . "Relevant quote")
  (encrypted_index . "abc123")))
```

### `greger-tree-sitter--extract-citation-entry (entry-node)`
**Input:** Single citation entry node (### URL + metadata)
**Output:** Single citation object with url, title, cited_text, encrypted_index fields

### `greger-tree-sitter--parse-content-with-citations (content citations)`
**Core citation parser** - Splits text containing `<cite>` tags into structured blocks.

**Input:**
- String with `<cite>text</cite>` tags
- List of citation objects

**Output:** List of text content blocks where cited text has citations attached:
```elisp
(((type . "text") (text . "Before cite"))
 ((type . "text") (text . "cited text") (citations . citation-list))
 ((type . "text") (text . "After cite")))
```

### `greger-tree-sitter--associate-citations-with-blocks (content-blocks citations)`
**Input:** List of content blocks and citations
**Output:** Updated content blocks with citations linked to cited text
**Behavior:** Finds text blocks containing `<cite>` tags and processes them

## Utility Functions

### `greger-tree-sitter--extract-section (section-node)`
**Section dispatcher** - Determines section type and calls appropriate extractor.

### `greger-tree-sitter--reorder-assistant-blocks (blocks)`
**Input:** List of assistant content blocks
**Output:** Reordered blocks (server_tool_use → web_search_tool_result → others)

### `greger-tree-sitter--extract-tool-param-value (param-node)`
**Input:** Tool parameter node (`<tool.id>value</tool.id>`)
**Output:** Trimmed parameter value string

### `greger-tree-sitter--extract-content (content-node)`
**Input:** Tree-sitter content node
**Output:** Trimmed text content string

### Navigation Utilities

- `greger-tree-sitter--find-child-by-type (node type)` - Find first child of given type
- `greger-tree-sitter--get-all-sections (root-node)` - Extract all section nodes
- `greger-tree-sitter--get-section-type (section-node)` - Get section type string
- `greger-tree-sitter--get-sections (root-node)` - Alternative section extractor

### Legacy Functions

- `greger-tree-sitter--message-has-cite-tags (message)` - Check for `<cite>` in string content
- `greger-tree-sitter--associate-citations (message citations)` - Simple citation association

## Key Data Structures

### Message Object
```elisp
((role . "user|assistant|system")
 (content . string-or-content-block-list))
```

### Content Block Types
```elisp
;; Text block (with optional citations)
((type . "text") (text . "content") (citations . citation-list))

;; Thinking block
((type . "thinking") (thinking . "internal thoughts"))

;; Tool use block
((type . "tool_use") (id . "id") (name . "name") (input . param-alist))

;; Tool result block
((type . "tool_result") (tool_use_id . "id") (content . "result"))

;; Server tool blocks (similar structure)
((type . "server_tool_use") ...)
((type . "web_search_tool_result") ...)
```

### Citation Object
```elisp
((type . "web_search_result_location")
 (url . "https://example.com")
 (title . "Page Title")
 (cited_text . "Relevant quote from source")
 (encrypted_index . "abc123"))
```

## Usage Examples

### Basic Conversation
```elisp
(greger-tree-sitter-parse "## USER:\n\nHello\n\n## ASSISTANT:\n\nHi!")
```

### Tool Use Workflow
```elisp
(greger-tree-sitter-parse "## TOOL USE:\n\nName: read-file\nID: tool_1\n\n### path\n\n<tool.tool_1>\nfile.txt\n</tool.tool_1>\n\n## TOOL RESULT:\n\nID: tool_1\n\n<tool.tool_1>\nFile contents\n</tool.tool_1>")
```

### Citations
```elisp
(greger-tree-sitter-parse "## ASSISTANT:\n\n<cite>Cited text</cite>\n\n## CITATIONS:\n\n### https://example.com\n\nTitle: Example\nCited text: Full quote\nEncrypted index: abc123")
```

This comprehensive documentation covers all functions in the greger-tree-sitter.el module, providing clear understanding of inputs, outputs, and the overall architecture for parsing greger conversation format.
