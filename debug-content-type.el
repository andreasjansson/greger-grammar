;;; Debug content type detection

(require 'json)

(defun greger-tree-sitter--parse-json-or-plain-content (content)
  "Parse CONTENT as JSON if it looks like JSON, otherwise return as plain text."
  (if (and (string-match-p "^\\s-*\\[\\|^\\s-*{" content)
           (condition-case nil
               (json-parse-string content :object-type 'alist :array-type 'list)
             (json-parse-error nil)))
      (json-parse-string content :object-type 'alist :array-type 'list)
    content))

(let* ((citations-content "[\n  {\n    \"type\": \"web_search_result\",\n    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",\n    \"title\": \"Claude Shannon - Wikipedia\",\n    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",\n    \"page_age\": \"April 30, 2025\"\n  }\n]")
       (basic-content "[\n  {\n    \"title\": \"Weather in San Francisco\",\n    \"url\": \"https://weather.com/sf\",\n    \"content\": \"Sunny, 72°F\"\n  }\n]")
       (parsed-citations (greger-tree-sitter--parse-json-or-plain-content citations-content))
       (parsed-basic (greger-tree-sitter--parse-json-or-plain-content basic-content)))

  (message "Citations content parsed: %S" parsed-citations)
  (message "Citations type check: %s"
           (if (and (listp parsed-citations)
                    (> (length parsed-citations) 0)
                    (listp (car parsed-citations))
                    (assoc 'type (car parsed-citations))
                    (string= (cdr (assoc 'type (car parsed-citations))) "web_search_result"))
               "web_search_tool_result"
             "server_tool_result"))

  (message "Basic content parsed: %S" parsed-basic)
  (message "Basic type check: %s"
           (if (and (listp parsed-basic)
                    (> (length parsed-basic) 0)
                    (listp (car parsed-basic))
                    (assoc 'type (car parsed-basic))
                    (string= (cdr (assoc 'type (car parsed-basic))) "web_search_result"))
               "web_search_tool_result"
             "server_tool_result")))
