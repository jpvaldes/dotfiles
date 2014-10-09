;; Enable flyspell for text and other files
(dolist (hooks '(text-mode-hook LaTeX-mode-hook))
  (add-hook hooks (lambda () (flyspell-mode 1))))
(dolist (hooks '(change-log-mode-hook log-edit-mode-hook python-mode-hook))
  (add-hook hooks (lambda () (flyspell-mode -1))))
(dolist (hooks '(emacs-lisp-mode-hook inferior-lisp-mode-hook))
  (add-hook hooks (lambda () (flyspell-prog-mode))))

; Found a possible workaround for flyspell slowness
(setq-default flyspell-issue-welcome-flag nil)
; http://www.brool.com/index.php/speeding-up-flyspell-region
(defadvice flyspell-region (around fast-flyspell-region)
  (flet ( (sit-for (x) t) )
    ad-do-it))
(ad-activate 'flyspell-region)

(provide 'init-flyspell)
