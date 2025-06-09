#!/usr/bin/env emacs --script
;;; test-validate-nodes.el --- Validate which node types exist

(setq debug-on-error t)
(load-file "grgfoo.el")

(when (treesit-ready-p 'greger)
  (message "Validating individual node type queries...")

  (let ((node-types '(
    "value"
    "url"
    "param_name"
    "html_comment"
    "text"
    "code_block"
    "inline_code"
    "cite_tag"
    "safe_shell_commands"
    "tool_start_tag"
    "tool_end_tag"
    "tool_content"
    "ERROR"
    )))

    (dolist (node-type node-types)
      (condition-case err
          (progn
            (treesit-query-validate 'greger `((,node-type) @test-face))
            (message "✓ %s - valid" node-type))
        (error (message "✗ %s - invalid: %s" node-type err))))))

(message "Validation completed")
