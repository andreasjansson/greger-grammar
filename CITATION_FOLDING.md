# Citation Folding in grgfoo-mode

This document describes the citation folding feature implemented in grgfoo-mode.

## Overview

Citation folding allows you to hide detailed citation information while preserving a summary view of the cited text. This makes it easier to read through conversation logs without being distracted by the full citation metadata.

## How It Works

### Default Folded View

When citation folding is enabled (which it is by default), the following transformations occur:

**Before (expanded):**
```
## ASSISTANT:

Einstein developed the theory of relativity

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

## ASSISTANT:

while Newton formulated the laws of motion

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789

## CITATIONS:

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789
```

**After (folded):**
```
## ASSISTANT:

Einstein developed the theory of relativity while Newton formulated the laws of motion

## CITATIONS:
[+2 citations, TAB to expand]
```

### Visual Indicators

- **Underlined text**: Citation text that is folded is displayed with an underline to indicate it can be expanded
- **Summary text**: The CITATIONS section shows a count of how many citations are folded

## Usage

### Expanding/Collapsing Citations

1. **Individual Citations**: Position your cursor on underlined citation text and press `TAB` to expand/collapse that specific citation
2. **Citations Section**: Position your cursor on the `## CITATIONS:` header and press `TAB` to expand/collapse the entire citations section

### Keyboard Shortcuts

- `TAB` - Toggle folding of citation at point, or fall back to normal indentation if not on a citation

## Implementation Details

### Font-Lock Functions

The folding is implemented using custom font-lock functions that:

1. Check if folding is enabled via `grgfoo-citation-folding-enabled`
2. Look for text properties to determine folding state:
   - `grgfoo-citation-expanded` for individual citations
   - `grgfoo-citations-expanded` for the citations section
3. Apply `invisible` text properties to hide content
4. Add visual indicators like underlines and summary text

### Text Properties Used

- `invisible`: Makes text invisible (values: `grgfoo-citation`, `grgfoo-citations`)
- `face`: Applies underline to citation text when folded
- `after-string`: Adds summary text after headers
- `grgfoo-citation-expanded`: Tracks expansion state of individual citations
- `grgfoo-citations-expanded`: Tracks expansion state of citations section

### Tree-sitter Queries

The implementation uses tree-sitter queries to identify:
- `(assistant (citation_entry))` - Individual citations in assistant responses
- `(citations)` - The main citations section

## Customization

### Variables

- `grgfoo-citation-folding-enabled` (default: `t`): Enable/disable citation folding
- `grgfoo-citation-summary-face` (default: `'underline`): Face used for folded citation text

### Disabling Citation Folding

To disable citation folding completely:

```elisp
(setq grgfoo-citation-folding-enabled nil)
```

Or disable it for a specific buffer:

```elisp
(setq-local grgfoo-citation-folding-enabled nil)
```

## Technical Notes

### Invisibility Specs

The mode adds two invisibility specs to the buffer:
- `grgfoo-citation`: For individual citation content
- `grgfoo-citations`: For the entire citations section

### Font-Lock Integration

Citation folding is implemented as a font-lock feature named `folding` and is enabled at font-lock level 2 (with headers).

### Tree-sitter Integration

The folding respects the tree-sitter parse tree structure, ensuring that folding boundaries align with the actual grammar structure of Greger files.
