;;;----------------------------------------;;;
;;; Flyspell
;;;----------------------------------------;;;
; Make german umlauts work.
(set-selection-coding-system 'utf-8)
; Extend the path in Emacs to find aspell just in case
(add-to-list 'exec-path "/usr/local/bin")
; find aspell and hunspell automatically
; The cond means it will use the first available (hunspell
; and then aspell)
(cond
 ((executable-find "aspell") (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=english"))
  (setq ispell-really-aspell t)
  ( message "Aspell has been found." ))
 ((executable-find "hunspell") (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (message "Hunspell has been found."))
)
(setq ispell-list-command "list"
      ispell-personal-dictionary "~/.emacs.d/personal-dict")
(ispell-change-dictionary "english") ; weird that I have to...
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
