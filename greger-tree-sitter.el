;;; greger-tree-sitter.el --- Parse greger format using tree-sitter -*- lexical-binding: t -*-

;; Author: Assistant
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides functionality to parse greger format files using tree-sitter
;; and convert them to the expected dialog format.

;;; Code:

(require 'treesit)
(require 'cl-lib)

(defun greger-tree-sitter-parse (content)
  "Parse CONTENT using tree-sitter and convert to dialog format."
  (with-temp-buffer
    (insert content)
    (when (treesit-language-available-p 'greger)
      (let ((parser (treesit-parser-create 'greger)))
        (when parser
          (let ((tree (treesit-parser-root-node parser)))
            (greger-tree-sitter-convert-node tree content)))))))

(defun greger-tree-sitter-convert-node (node content)
  "Convert a tree-sitter NODE with CONTENT to dialog format."
  (let ((node-type (treesit-node-type node))
        (children (treesit-node-children node)))
    (cond
     ((string= node-type "source_file")
      (greger-tree-sitter-convert-source-file children content))
     (t
      (error "Unknown node type: %s" node-type)))))

(defun greger-tree-sitter-convert-source-file (children content)
  "Convert source file CHILDREN with CONTENT to dialog format."
  (let ((result nil))
    (dolist (child children)
      (when (treesit-node-p child)
        (let ((child-type (treesit-node-type child)))
          (cond
           ((string= child-type "user")
            (push (greger-tree-sitter-convert-user child content) result))
           ((string= child-type "assistant")
            (push (greger-tree-sitter-convert-assistant child content) result))
           ((string= child-type "system")
            (push (greger-tree-sitter-convert-system child content) result))
           ((string= child-type "thinking")
            ;; Thinking sections are merged into the next assistant section
            ;; For now, we'll handle this in a post-processing step
            nil)
           (t
            (message "Warning: Unknown section type: %s" child-type))))))
    (nreverse result)))

(defun greger-tree-sitter-convert-user (node content)
  "Convert a user NODE with CONTENT to dialog format."
  (let ((text-content (greger-tree-sitter-extract-text node content)))
    `((role . "user")
      (content . ,text-content))))

(defun greger-tree-sitter-convert-assistant (node content)
  "Convert an assistant NODE with CONTENT to dialog format."
  (let ((text-content (greger-tree-sitter-extract-text node content)))
    `((role . "assistant")
      (content . ,text-content))))

(defun greger-tree-sitter-convert-system (node content)
  "Convert a system NODE with CONTENT to dialog format."
  (let ((text-content (greger-tree-sitter-extract-text node content)))
    `((role . "system")
      (content . ,text-content))))

(defun greger-tree-sitter-extract-text (node content)
  "Extract text content from NODE using CONTENT string."
  (let ((children (treesit-node-children node)))
    (if children
        (let ((text-node (car children)))
          (when (and (treesit-node-p text-node)
                     (string= (treesit-node-type text-node) "text"))
            (let ((start (treesit-node-start text-node))
                  (end (treesit-node-end text-node)))
              (string-trim (substring content start end)))))
      "")))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
