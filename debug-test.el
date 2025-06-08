(load-file "./greger-tree-sitter.el")

(let* ((text "## USER:

Read the file hello.txt

## TOOL USE:

Name: read-file
ID: toolu_123

### path

<tool.toolu_123>
hello.txt
</tool.toolu_123>")
       (result (greger-tree-sitter-parse text)))
  (message "Result:")
  (pp result))
