;;; test-interactive-gui.el --- Interactive GUI test for the major mode

;; This script can be run with: emacs --load test-interactive-gui.el

;; Load the grgfoo mode
(load-file "grgfoo.el")

;; Create a new buffer and activate the mode
(switch-to-buffer "demo.grgfoo")
(grgfoo-mode)

;; Insert some sample content
(insert "## USER:")
(newline)
(insert "What is the capital of France?")
(newline)
(newline)
(insert "## ASSISTANT:")
(newline)
(insert "The capital of France is Paris.")
(newline)
(newline)
(insert "## TOOL USE:")
(newline)
(insert "Name: search_web")
(newline)
(insert "ID: 1234")
(newline)
(insert "### query")
(newline)
(insert "Paris France capital city")

;; Position cursor at end
(goto-char (point-max))

;; Display message
(message "✓ Demo content loaded! The major mode is now active.")
(message "✓ Try typing more content and pressing Enter - it should not crash.")
(message "✓ Syntax highlighting should be visible.")
(message "✓ You can close this buffer when done testing.")
