;;; debug-section-order.el --- Debug section processing order -*- lexical-binding: t -*-

(load-file "./greger-tree-sitter.el")

;; Override section processing to add debug output
(defun greger-tree-sitter--process-sections-with-citations (sections)
  "Debug version that prints what sections it processes."
  (message "DEBUG: Processing %d sections" (length sections))
  (let ((messages '())
        (current-assistant-blocks '())
        (i 0))

    (while (< i (length sections))
      (let* ((section (nth i sections))
             (section-type (greger-tree-sitter--get-section-type section)))

        (message "DEBUG: Section %d: type = %s" i section-type)

        (cond
         ;; User section - flush any pending assistant blocks and add user message
         ((equal section-type "user_section")
          (message "DEBUG: Processing user section")
          (when current-assistant-blocks
            (message "DEBUG: Flushing %d assistant blocks" (length current-assistant-blocks))
            (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
              (push `((role . "assistant") (content . ,assistant-content)) messages))
            (setq current-assistant-blocks '()))
          (let ((message (greger-tree-sitter--extract-section section)))
            (message "DEBUG: Extracted user message: %S" message)
            (when message (push message messages))))

         ;; Assistant-related sections - collect content blocks
         ((member section-type '("assistant_section" "thinking_section" "tool_use_section"
                                 "server_tool_use_section" "server_tool_result_section"))
          (message "DEBUG: Processing assistant-related section: %s" section-type)
          (let ((message (greger-tree-sitter--extract-section section)))
            (when message
              (let ((content (alist-get 'content message)))
                (message "DEBUG: Assistant content: %S" content)
                (if (listp content)
                    ;; Add all content blocks
                    (setq current-assistant-blocks (append current-assistant-blocks content))
                  ;; Store string content as-is for now (will be processed later)
                  (when (and (stringp content) (> (length (string-trim content)) 0))
                    (setq current-assistant-blocks (append current-assistant-blocks `(((type . "text") (text . ,content)))))))))))

         ;; Citations section - associate with pending assistant blocks
         ((equal section-type "citations_section")
          (message "DEBUG: Processing citations section")
          (when current-assistant-blocks
            (message "DEBUG: Associating citations with %d assistant blocks" (length current-assistant-blocks))
            (let ((citations (greger-tree-sitter--extract-citations-section section)))
              (message "DEBUG: Extracted %d citations" (length citations))
              ;; Find text blocks with cite tags and associate citations
              (setq current-assistant-blocks
                    (greger-tree-sitter--associate-citations-with-blocks current-assistant-blocks citations)))))

         (t
          (message "DEBUG: Unknown section type: %s" section-type)))

      (setq i (1+ i))))

    ;; Flush any remaining assistant blocks
    (when current-assistant-blocks
      (message "DEBUG: Final flush of %d assistant blocks" (length current-assistant-blocks))
      (let ((assistant-content (greger-tree-sitter--finalize-assistant-content current-assistant-blocks)))
        (push `((role . "assistant") (content . ,assistant-content)) messages)))

    (message "DEBUG: Final messages: %S" (nreverse messages))
    (nreverse messages)))

;; Test with simple citation case
(let ((test-text "## USER:

Test

## ASSISTANT:

<cite>cited text</cite>

## CITATIONS:

### https://example.com

Title: Example
Cited text: cited text
Encrypted index: abc123"))
  (message "\n=== Testing section processing ===")
  (condition-case err
      (let ((result (greger-tree-sitter-parse test-text)))
        (message "Final result: %S" result))
    (error
     (message "Error: %s" (error-message-string err)))))
