;;; greger-tree-sitter.el --- Parse greger format using manual parsing -*- lexical-binding: t -*-

;; Author: Assistant
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides functionality to parse greger format files
;; and convert them to the expected dialog format.
;; Currently uses manual parsing, but can be upgraded to tree-sitter later.

;;; Code:

(require 'cl-lib)

(defun greger-tree-sitter-parse (content)
  "Parse CONTENT and convert to dialog format."
  (let ((sections (greger-tree-sitter-split-sections content)))
    (greger-tree-sitter-convert-sections sections)))

(defun greger-tree-sitter-split-sections (content)
  "Split CONTENT into sections based on ## headers."
  (let ((sections nil)
        (lines (split-string content "\n"))
        (current-section nil)
        (current-type nil)
        (current-content nil))

    (dolist (line lines)
      (cond
       ;; Check for section headers
       ((string-match "^## \\(USER\\|ASSISTANT\\|SYSTEM\\|THINKING\\|TOOL USE\\|TOOL RESULT\\):" line)
        ;; Save previous section if exists
        (when current-type
          (push (list current-type (string-trim (mapconcat 'identity (nreverse current-content) "\n"))) sections))
        ;; Start new section
        (setq current-type (match-string 1 line))
        (setq current-content nil))
       ;; Add content to current section
       (current-type
        (push line current-content))
       ;; Content before any section header (shouldn't happen in valid greger)
       (t
        (unless (string-empty-p (string-trim line))
          (message "Warning: Content outside of section: %s" line)))))

    ;; Don't forget the last section
    (when current-type
      (push (list current-type (string-trim (mapconcat 'identity (nreverse current-content) "\n"))) sections))

    (nreverse sections)))

(defun greger-tree-sitter-convert-sections (sections)
  "Convert SECTIONS to dialog format."
  (let ((result nil)
        (pending-thinking nil)
        (pending-tool-use nil)
        (pending-tool-results nil))

    (dolist (section sections)
      (let ((type (car section))
            (content (cadr section)))
        (cond
         ((string= type "USER")
          ;; Flush any pending content before user message
          (when pending-thinking
            (push (greger-tree-sitter-make-assistant-with-thinking pending-thinking nil) result)
            (setq pending-thinking nil))
          (push `((role . "user") (content . ,content)) result))

         ((string= type "SYSTEM")
          (push `((role . "system") (content . ,content)) result))

         ((string= type "THINKING")
          (setq pending-thinking content))

         ((string= type "TOOL USE")
          (setq pending-tool-use (greger-tree-sitter-parse-tool-use content)))

         ((string= type "TOOL RESULT")
          (push (greger-tree-sitter-parse-tool-result content) pending-tool-results))

         ((string= type "ASSISTANT")
          ;; Create assistant message with any pending content
          (let ((assistant-content nil))

            ;; Add thinking if present
            (when pending-thinking
              (push `((type . "thinking") (thinking . ,pending-thinking)) assistant-content)
              (setq pending-thinking nil))

            ;; Add tool use if present
            (when pending-tool-use
              (push pending-tool-use assistant-content)
              (setq pending-tool-use nil))

            ;; Add text content if present
            (when (and content (not (string-empty-p content)))
              (push `((type . "text") (text . ,content)) assistant-content))

            ;; Create the assistant message
            (if assistant-content
                (push `((role . "assistant") (content . ,(nreverse assistant-content))) result)
              (push `((role . "assistant") (content . ,content)) result)))

         (t
          (message "Warning: Unknown section type: %s" type)))))

    ;; Handle any pending tool results
    (dolist (tool-result (nreverse pending-tool-results))
      (push `((role . "user") (content . (,tool-result))) result))

    (nreverse result)))

(defun greger-tree-sitter-parse-tool-use (content)
  "Parse TOOL USE section CONTENT."
  (let ((lines (split-string content "\n"))
        (name nil)
        (id nil)
        (input nil)
        (current-param nil)
        (current-param-content nil)
        (in-tool-block nil))

    (dolist (line lines)
      (cond
       ;; Parse Name
       ((string-match "^Name: \\(.+\\)" line)
        (setq name (match-string 1 line)))

       ;; Parse ID
       ((string-match "^ID: \\(.+\\)" line)
        (setq id (match-string 1 line)))

       ;; Parse parameter name
       ((string-match "^### \\(.+\\)" line)
        ;; Save previous parameter if exists
        (when current-param
          (push (cons (intern current-param)
                     (string-trim (mapconcat 'identity (nreverse current-param-content) "\n")))
                input))
        (setq current-param (match-string 1 line))
        (setq current-param-content nil)
        (setq in-tool-block nil))

       ;; Tool block start
       ((string-match "^<tool\\." line)
        (setq in-tool-block t))

       ;; Tool block end
       ((string-match "^</tool\\." line)
        (setq in-tool-block nil))

       ;; Content inside tool block
       (in-tool-block
        (push line current-param-content))

       ;; Other content (ignore for now)
       (t nil)))

    ;; Don't forget the last parameter
    (when current-param
      (push (cons (intern current-param)
                 (string-trim (mapconcat 'identity (nreverse current-param-content) "\n")))
            input))

    `((type . "tool_use")
      (id . ,id)
      (name . ,name)
      (input . ,(nreverse input)))))

(defun greger-tree-sitter-parse-tool-result (content)
  "Parse TOOL RESULT section CONTENT."
  (let ((lines (split-string content "\n"))
        (id nil)
        (result-content nil)
        (in-tool-block nil))

    (dolist (line lines)
      (cond
       ;; Parse ID
       ((string-match "^ID: \\(.+\\)" line)
        (setq id (match-string 1 line)))

       ;; Tool block start
       ((string-match "^<tool\\." line)
        (setq in-tool-block t))

       ;; Tool block end
       ((string-match "^</tool\\." line)
        (setq in-tool-block nil))

       ;; Content inside tool block
       (in-tool-block
        (push line result-content))

       ;; Other content (ignore for now)
       (t nil)))

    `((type . "tool_result")
      (tool_use_id . ,id)
      (content . ,(string-trim (mapconcat 'identity (nreverse result-content) "\n"))))))

(defun greger-tree-sitter-make-assistant-with-thinking (thinking text)
  "Create assistant message with THINKING and optional TEXT."
  (let ((content (list `((type . "thinking") (thinking . ,thinking)))))
    (when (and text (not (string-empty-p text)))
      (push `((type . "text") (text . ,text)) content))
    `((role . "assistant") (content . ,(nreverse content)))))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
