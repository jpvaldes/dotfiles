;;;----------------------------------------;;;
;;; Ispell Setup
;;;----------------------------------------;;;
; Extend the path in Emacs to find aspell just in case
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin"))
; find aspell and hunspell automatically
; lets do some spellying mistages 
(cond
 ((executable-find "ispell")
  (setq ispell-program-name "ispell")
  (setq ispell-dictionary "english")
  (message "Using Ispell"))
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=english"))
  (setq ispell-really-aspell t)
  (setq ispell-list-command "--list"
        ispell-personal-dictionary "~/.emacs.d/personal-dict")
  (setq ispell-dictionary "english")
  (message "Aspell has been found." ))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setq ispell-local-dictionary-alist
        '(
          (nil
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_US")
           nil
           utf-8)
          ("american"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_US")
           nil
           utf-8)
          ("english"
           "[[:alpha:]]"
           "[^[:alpha:]]"
           "[']"
           t
           ("-d" "en_GB")
           nil
           utf-8)
          ("deutsch"
           "[[:alpha:]ÄÖÜéäöüß]"
           "[^[:alpha:]ÄÖÜéäöüß]"
           "[']"
           t
           ("-d" "de_DE")
           nil
           iso-8859-1)
          ("spanish"
           "[[:alpha:]üÜáéíóúÁÉÍÓÚñÑ]"
           "[^[:alpha:]üÜáéíóúÁÉÍÓÚñÑ]"
           "[']"
           t
           ("-d" "es_ES")
           nil
           iso-8859-1)
          ))
  (require-package 'rw-language-and-country-codes)
  (require-package 'rw-ispell)
  (require-package 'rw-hunspell)
  (setq ispell-dictionary "american")
  (setq rw-hunspell-default-dictionary ispell-dictionary)
  (setq rw-hunspell-dicpath-list (quote ("~/Library/Spelling")))
  (setq rw-hunspell-make-dictionary-menu t)
  (setq rw-hunspell-use-rw-ispell t)
  (eval-after-load "ispell"
    '(progn
       (require 'rw-language-and-country-codes)
       (require 'rw-ispell)
       (require 'rw-hunspell)
       ))
  (message "Hunspell has been found: using it."))
)
 
(provide 'init-ispell)
