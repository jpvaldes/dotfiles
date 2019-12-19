(require-package 'yasnippet)

;;;----------------------------------------;;;
;;; Yasnippets
;;;----------------------------------------;;;
;(add-to-list 'yas/root-directory "/Users/valdesj/.emacs.d/yasnippet-snippets")
(require 'yasnippet)
;(setq yas-snippet-dirs '("/Users/valdesj/.emacs.d/yasnippet-snippets"))
(setq yas-fallback-behavior 'return-nil)
(setq yas-also-auto-indent-first-line t)
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt))

;(yas-global-mode 1)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'yas-minor-mode)

(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "yasnippet-snippets"))

(yas-reload-all)

(provide 'init-yasnippet)
