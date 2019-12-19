;;; init-markdown.el --- markdown mode configuration

;;; Commentary:

;;; Code:

(lazy-major-mode "\\.md$" markdown-mode)

;; Markdown and Pandoc export
(setq markdown-command "pandoc --smart --from=markdown --to=html" )
; (setq-markdown-css-path (expand-file-name "markdown.css" jose/cssdir))

(provide 'init-markdown)
