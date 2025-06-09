;;; greger-tree-sitter.el --- Parse greger format using tree-sitter -*- lexical-binding: t -*-

;; Author: Assistant
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (tsc "0.18.0"))

;;; Commentary:

;; This package provides functionality to parse greger format files using tree-sitter
;; and convert them to the expected dialog format.

;;; Code:

(require 'tsc)
(require 'cl-lib)

(defvar greger-tree-sitter-language nil
  "Tree-sitter language object for greger.")

(defun greger-tree-sitter-init ()
  "Initialize the tree-sitter language for greger."
  (unless greger-tree-sitter-language
    (setq greger-tree-sitter-language
          (tsc-load-language "libtree-sitter-greger"))))

(defun greger-tree-sitter-parse (content)
  "Parse CONTENT using tree-sitter and convert to dialog format."
  (greger-tree-sitter-init)
  (let* ((parser (tsc-make-parser))
         (tree nil)
         (result nil))
    (tsc-set-language parser greger-tree-sitter-language)
    (setq tree (tsc-parse-string parser content))
    (when tree
      (let ((root-node (tsc-root-node tree)))
        (setq result (greger-tree-sitter-convert-node root-node content))))
    result))

(defun greger-tree-sitter-convert-node (node content)
  "Convert a tree-sitter NODE with CONTENT to dialog format."
  (let ((node-type (tsc-node-type node))
        (children (tsc-get-children node)))
    (cond
     ((string= node-type "source_file")
      (greger-tree-sitter-convert-source-file children content))
     (t
      (error "Unknown node type: %s" node-type)))))

(defun greger-tree-sitter-convert-source-file (children content)
  "Convert source file CHILDREN with CONTENT to dialog format."
  (let ((result nil))
    (dolist (child children)
      (let ((child-type (tsc-node-type child)))
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
          (message "Warning: Unknown section type: %s" child-type)))))
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
  (let ((children (tsc-get-children node)))
    (if children
        (let ((text-node (car children)))
          (when (string= (tsc-node-type text-node) "text")
            (let ((start (tsc-node-start-byte text-node))
                  (end (tsc-node-end-byte text-node)))
              (string-trim (substring content start end)))))
      "")))

(provide 'greger-tree-sitter)

;;; greger-tree-sitter.el ends here
