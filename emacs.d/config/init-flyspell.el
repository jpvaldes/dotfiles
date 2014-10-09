;; Enable flyspell for text and other files
(dolist (hooks '(text-mode-hook LaTeX-mode-hook))
  (add-hook hooks (lambda () (flyspell-mode 1))))
(dolist (hooks '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hooks (lambda () (flyspell-mode -1))))
(dolist (hooks '(emacs-lisp-mode-hook inferior-lisp-mode-hook python-mode-hook))
  (add-hook hooks (lambda () (flyspell-prog-mode))))

(provide 'init-flyspell)
