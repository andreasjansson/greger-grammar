# Greger Tree-sitter Grammar

A tree-sitter grammar for parsing the greger conversation format, which is a markdown-like syntax for AI conversations with tools, thinking sections, and citations.

## Features

The grammar supports parsing:

- **Section Headers**: `## USER:`, `## ASSISTANT:`, `## SYSTEM:`, `## THINKING:`, etc.
- **Tool Use**: Tool calls with parameters in `<tool.ID>` blocks
- **Tool Results**: Tool responses with matching IDs
- **Server Tool Use/Results**: Server-side tool operations
- **Citations**: Reference sections with URLs and metadata
- **Code Blocks**: Triple and single backtick code blocks
- **Special Tags**: `<cite>`, `<include>`, `<safe-shell-commands>`
- **HTML Comments**: `<!-- -->`

## Key Innovation: ID-Matched Tool Blocks

The grammar uses a custom external scanner to ensure tool blocks with matching IDs are parsed correctly:

```
<tool.abc123>
Content here, including fake tags like:
<tool.def456>
This doesn't break parsing!
</tool.def456>
</tool.abc123>
```

Only the matching `</tool.abc123>` closes the block.

## Files

- `grammar.js` - Main tree-sitter grammar definition
- `src/scanner.c` - External scanner for tool block ID matching
- `greger-tree-sitter.el` - Elisp integration functions
- `test-integration.el` - Test script for Elisp integration

## Test Files

- `test_simple.greger` - Basic conversation
- `test_complex.greger` - Tool use example
- `test_comprehensive.greger` - All features
- `test_nested_tools.greger` - Nested tool content test

## Building

```bash
# Generate the parser
tree-sitter generate

# Test parsing
tree-sitter parse test_simple.greger
tree-sitter parse test_complex.greger
tree-sitter parse test_comprehensive.greger
tree-sitter parse test_nested_tools.greger
```

## Usage in Elisp

```elisp
(require 'greger-tree-sitter)

;; Parse greger format text
(greger-tree-sitter-parse "## USER:\n\nHello!")

;; Returns same format as greger-parser-parse-dialog-messages-only:
;; (((role . "user") (content . "Hello!")))
```

## Current Status

The grammar successfully parses:
- ✅ Section headers and content
- ✅ Tool use/result blocks with ID matching
- ✅ Basic content types
- ⚠️  Some content parsing needs refinement (minor ERROR nodes)

The core structure is working and can replace the regex-based parser in greger.el.

## Integration with greger.el

To use this as a replacement for the current parser:

1. Build the tree-sitter grammar and install the `.so` file
2. Load `greger-tree-sitter.el`
3. Replace calls to `greger-parser-parse-dialog-messages-only` with `greger-tree-sitter-parse`

The return format is identical, so it's a drop-in replacement.

## Testing

Run the comprehensive test:

```bash
tree-sitter parse test_comprehensive.greger
```

This file includes all major features:
- System content with safe shell commands
- User content with citations and includes
- Thinking sections
- Server tool use and results
- Assistant responses with citations
- Code blocks and comments
- Citation metadata

The parser correctly identifies section boundaries and extracts structured content while handling nested tool-like syntax properly.
