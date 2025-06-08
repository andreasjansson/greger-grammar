(load-file "./greger-tree-sitter.el")

(let* ((text "## USER:

Here's some code:

<!-- comment -->
<!-- multi
line

comment -->

```
<!-- comment should be included -->
## ASSISTANT:
This should not be parsed as a section header
## TOOL USE:
Neither should this
```

What do you think?")
       (result (greger-tree-sitter-parse text)))
  (message "Result:")
  (pp result))
