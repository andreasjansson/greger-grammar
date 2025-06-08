(load-file "./greger-tree-sitter.el")

(let* ((text "## USER:

Search for python files containing 'def main'

## TOOL USE:

Name: ripgrep
ID: toolu_456

### pattern

<tool.toolu_456>
def main
</tool.toolu_456>

### file-type

<tool.toolu_456>
py
</tool.toolu_456>

### context-lines

<tool.toolu_456>
2
</tool.toolu_456>")
       (result (greger-tree-sitter-parse text)))
  (message "Result:")
  (pp result))
