;;;----------------------------------------;;;
;;; Ispell Setup
;;;----------------------------------------;;;
; Extend the path in Emacs to find aspell just in case
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin"))
; find aspell and hunspell automatically
(cond
 ((executable-find "ispell")
  (setq ispell-program-name "ispell")
  (message "Using Ispell"))
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
 
(provide 'init-ispell)
