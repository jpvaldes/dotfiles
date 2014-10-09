;;;----------------------------------------;;;
;;; Ispell Setup
;;;----------------------------------------;;;
; Extend the path in Emacs to find aspell just in case
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin"))
; find aspell and hunspell automatically
(cond
 ((executable-find "ispell")
  (setq ispell-program-name "ispell"))
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=english"))
  (setq ispell-really-aspell t)
  (setq ispell-list-command "--list"
        ispell-personal-dictionary "~/.emacs.d/personal-dict")
  (message "Aspell has been found." ))
 ((executable-find "hunspell") (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (message "Hunspell has been found."))
)
(setq ispell-dictionary "english")
;; Enable flyspell for text and other files
;; The way autocomplete is setup, when a word is mispelled
;; do alt-<tab> to cycle between corrected options
;(dolist (hooks '(text-mode-hook
;		 LaTeX-mode-hook))
;  (add-hook hooks (lambda () (flyspell-mode 1))))
;(add-hook 'org-mode-hook '(lambda () (flyspell-prog-mode)))
;(dolist (modes '(emacs-lisp-mode-hook
;		 inferior-lisp-mode-hook
;		 python-mode-hook))
;  (add-hook modes '(lambda () (flyspell-prog-mode))))
 
(provide 'init-ispell)
