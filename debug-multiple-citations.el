;;; debug-multiple-citations.el --- Debug multiple citations output -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

(defun debug-multiple-citations ()
  "Debug the multiple citations case to see detailed output."
  (let ((markdown "## USER:

Tell me about Einstein and Newton

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_789

### query

<tool.srvtoolu_789>
Einstein Newton physics
</tool.srvtoolu_789>

## SERVER TOOL RESULT:

ID: srvtoolu_789

<tool.srvtoolu_789>
Search results about Einstein and Newton
</tool.srvtoolu_789>

## ASSISTANT:

<cite>Einstein developed the theory of relativity</cite>

## CITATIONS:

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

## ASSISTANT:

while <cite>Newton formulated the laws of motion</cite>

## CITATIONS:

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789"))

    (message "\n=== Debugging multiple citations ===")
    (condition-case err
        (let ((result (greger-tree-sitter-parse markdown)))
          (message "Number of messages: %d" (length result))
          (dotimes (i (length result))
            (let ((msg (nth i result)))
              (message "\nMessage %d:" (1+ i))
              (message "  Role: %s" (alist-get 'role msg))
              (let ((content (alist-get 'content msg)))
                (if (stringp content)
                    (message "  Content: %s" content)
                  (progn
                    (message "  Content blocks: %d" (length content))
                    (dotimes (j (length content))
                      (let ((block (nth j content)))
                        (message "    Block %d:" (1+ j))
                        (message "      Type: %s" (alist-get 'type block))
                        (when (string= (alist-get 'type block) "text")
                          (message "      Text: %s" (alist-get 'text block))
                          (when (alist-get 'citations block)
                            (message "      Citations: %d" (length (alist-get 'citations block)))))
                        (when (string= (alist-get 'type block) "server_tool_use")
                          (message "      ID: %s" (alist-get 'id block))
                          (message "      Name: %s" (alist-get 'name block)))
                        (when (string= (alist-get 'type block) "web_search_tool_result")
                          (message "      Tool Use ID: %s" (alist-get 'tool_use_id block)))))))))))
      (error
       (message "❌ Error: %s" (error-message-string err))
       nil))))

;; Run debug
(if (treesit-ready-p 'greger)
    (debug-multiple-citations)
  (message "Tree-sitter greger parser not available"))
