# Greger Grammar

[![CI](https://github.com/andreasjansson/greger-grammar/actions/workflows/ci.yml/badge.svg)](https://github.com/andreasjansson/greger-grammar/actions/workflows/ci.yml)

Tree-sitter grammar for [greger.el](https://github.com/andreasjansson/greger.el) - a structured markdown format for AI conversations.

## Features

- **Section Headers**: `# SYSTEM`, `# USER`, `# ASSISTANT`, `# THINKING`, etc.
- **Tool Use**: Support for `<tool.name>` tags with structured parameters
- **Code Blocks**: Triple backtick code blocks with language support
- **Eval Tags**: `<eval>` tags for executable content (system/user contexts only)
- **Safe Shell Commands**: `<safe-shell-commands>` blocks
- **HTML Comments**: Standard `<!-- -->` comment support
- **Citations**: Structured citation entries with URLs and metadata

## Installation

```bash
npm install tree-sitter-greger
```

## Usage

### Node.js

```javascript
const Parser = require('tree-sitter');
const Greger = require('tree-sitter-greger');

const parser = new Parser();
parser.setLanguage(Greger);

const sourceCode = `# SYSTEM

You are a helpful assistant.

<eval>test</eval>

# USER

Hello world!

# ASSISTANT

Hi there!`;

const tree = parser.parse(sourceCode);
console.log(tree.rootNode.toString());
```

### Tree-sitter CLI

```bash
# Install tree-sitter CLI
npm install -g tree-sitter-cli

# Generate parser
tree-sitter generate

# Run tests
tree-sitter test

# Parse a file
tree-sitter parse example.greger
```

## Grammar Structure

The grammar supports these main constructs:

- **Sections**: `# SYSTEM`, `# USER`, `# ASSISTANT`, `# THINKING`, `# TOOL USE`, `# TOOL RESULT`
- **Tool Elements**: `<tool.function_name>`, parameters, and content
- **Eval Tags**: `<eval>`, `<eval bash>`, `<eval python>` (system/user only)
- **Safe Commands**: `<safe-shell-commands>` blocks
- **Code Blocks**: ```language code blocks
- **Inline Code**: `inline code`
- **HTML Comments**: `<!-- comments -->`
- **Citations**: URL-based citation entries

## Development

```bash
# Clone the repository
git clone https://github.com/andreasjansson/greger-grammar.git
cd greger-grammar

# Install dependencies
npm install

# Generate parser
tree-sitter generate

# Run tests
tree-sitter test

# Test Node.js bindings
cd bindings/node && npm test
```

## Testing

The project includes comprehensive test coverage:

- **Unit Tests**: `tree-sitter test` runs all corpus tests
- **CI/CD**: GitHub Actions test on Ubuntu, macOS, and Windows
- **Node.js Versions**: Tested on Node.js 16, 18, and 20
- **Grammar Validation**: Automated conflict detection

## License

MIT
