;;; init-evil.el --- Evil config

;;; Comentary:

;;; Code:

;;;-------------------- EVIL--------------------;;;

; Put setq customizations before loading evil as
; suggested in the manual
(setq evil-search-module 'evil-search)
; Cursor:
(setq evil-normal-state-cursor '("OliveDrab3" box))
(setq evil-insert-state-cursor '("red" hbar))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("red" hbar))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-emacs-state-cursor '("blue" box))

;; State tags
(setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "red" :foreground "black")))
      evil-visual-state-tag (propertize "V" 'face '((:background "orange" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "blue")))
      evil-motion-state-tag (propertize "M" 'face '((:background "purple"))))

;; Cursor moves backwards when exiting insert mode
;; I found this irritating
(setq evil-move-cursor-back nil)

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-numbers)
(require-package 'evil-visualstar)
(require-package 'evil-indent-textobject)
(require-package 'evil-matchit)
(require-package 'evil-surround)
(require-package 'evil-nerd-commenter)
(require-package 'evil-exchange)

(require 'evil-leader)
(require 'evil)
(require 'evil-surround)
(require 'evil-nerd-commenter)
(require 'evil-indent-textobject)
(require 'evil-visualstar)

;; In order to change cursor color while using evil
;;(setq evil-default-cursor t)
;; Enable evil-leader before evil-mode as per instructions.
(global-evil-leader-mode t)
;;; be eVIl!! http://gitorious.org/evil
(evil-mode 1)
;;; Even more evil, use evil-surround, the evil version of surround.vim.
(global-evil-surround-mode t)

(evil-exchange-install)
(global-evil-matchit-mode t)

;;;;-------------------- EVIL CONFIG END --------------------;;;

(provide 'init-evil)
