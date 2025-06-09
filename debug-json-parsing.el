;;; Debug JSON parsing

(require 'json)

(defun test-json-parsing ()
  "Test JSON parsing for different server tool result formats."
  (let* ((citations-json "[\n  {\n    \"type\": \"web_search_result\",\n    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",\n    \"title\": \"Claude Shannon - Wikipedia\",\n    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",\n    \"page_age\": \"April 30, 2025\"\n  }\n]")
         (basic-json "[\n  {\n    \"title\": \"Weather in San Francisco\",\n    \"url\": \"https://weather.com/sf\",\n    \"content\": \"Sunny, 72°F\"\n  }\n]")
         (parsed-citations (json-parse-string citations-json :object-type 'alist :array-type 'list))
         (parsed-basic (json-parse-string basic-json :object-type 'alist :array-type 'list)))

    (message "Citations JSON:")
    (pp parsed-citations)
    (message "Type check: %s" (if (and (listp parsed-citations)
                                      (> (length parsed-citations) 0)
                                      (listp (car parsed-citations))
                                      (assoc 'type (car parsed-citations))
                                      (string= (cdr (assoc 'type (car parsed-citations))) "web_search_result"))
                                 "web_search_tool_result"
                                 "server_tool_result"))

    (message "Basic JSON:")
    (pp parsed-basic)
    (message "Type check: %s" (if (and (listp parsed-basic)
                                      (> (length parsed-basic) 0)
                                      (listp (car parsed-basic))
                                      (assoc 'type (car parsed-basic))
                                      (string= (cdr (assoc 'type (car parsed-basic))) "web_search_result"))
                                 "web_search_tool_result"
                                 "server_tool_result"))))

(test-json-parsing)
