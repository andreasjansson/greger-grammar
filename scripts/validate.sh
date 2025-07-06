#!/bin/bash

# Validate greger-grammar setup
set -e

echo "🔧 Validating greger-grammar setup..."

# Check if tree-sitter CLI is available
if ! command -v tree-sitter &> /dev/null; then
    echo "❌ tree-sitter CLI not found. Install with: npm install -g tree-sitter-cli"
    exit 1
fi

echo "✅ tree-sitter CLI found"

# Validate grammar.js syntax
echo "🔍 Validating grammar.js syntax..."
node -c grammar.js
echo "✅ grammar.js syntax is valid"

# Generate parser
echo "🏗️  Generating parser..."
tree-sitter generate
echo "✅ Parser generated successfully"

# Run tests
echo "🧪 Running test suite..."
tree-sitter test
echo "✅ All tests passed"

# Test basic parsing
echo "📝 Testing basic parsing..."
cat > test_validation.txt << 'EOF'
# SYSTEM

You are a helpful assistant.

<eval>test content</eval>

<safe-shell-commands>
echo hello
<eval bash>echo world</eval>
</safe-shell-commands>

# USER

<eval>user eval</eval>

Hello!

# ASSISTANT

<eval>this should be text</eval>

Hi there!
EOF

tree-sitter parse test_validation.txt > /dev/null
echo "✅ Basic parsing works"

# Verify eval functionality
if tree-sitter parse test_validation.txt | grep -q "eval_start_tag"; then
    echo "✅ Eval functionality working"
else
    echo "❌ Eval functionality not working"
    exit 1
fi

# Cleanup
rm test_validation.txt

echo ""
echo "🎉 All validations passed! greger-grammar is ready for use."
echo ""
echo "Next steps:"
echo "  - Push to GitHub to trigger CI"
echo "  - Use 'npm test' to run Node.js binding tests"
echo "  - Use 'tree-sitter playground' to test interactively"
