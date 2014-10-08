(require-package 'smartparens)
(require 'smartparens-config)
;;------------------------------------------------------------;;
;;          Smartparens
;;------------------------------------------------------------;;
;; Try the alternative to autopair
(smartparens-global-mode 1)
;; Stopping from completing ' in elisp, which is annoying
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;;------------------------------------------------------------;;

(setq sp-autoescape-string-quote nil)

;; fix conflict where smartparens clobbers yas' key bindings
(after 'yasnippet
  (defadvice yas-expand (before advice-for-yas-expand activate)
    (sp-remove-active-pair-overlay)))

(provide 'init-smartparens)
