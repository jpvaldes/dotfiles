;;; init-autocomplete.el --- Auto-complete config

;;; Commentary:

;;; Code:

;;; ----------------------------------------------------------------------
;;; Autocomplete
;;;
(require-package 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(setq ac-auto-show-menu t)
;;; It wasn't working on Mac OS X
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.25 ; or t for instant
      ac-auto-start 3 ; wait for 3 characters to start, otherwise t
      ac-delay 0.125
      ac-ignore-case nil
      ac-candidate-menu-min 2
      ac-limit 10
      ac-use-quick-help t
      ac-quick-help-delay 0.3
      ac-quick-help-height 30
      ac-show-menu-immediately-on-auto-complete t
      ac-use-fuzzy t
      ac-fuzzy-cursor-color "magenta4"
      ac-menu-height 6)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-completing-map "C-<" 'ac-stop) ; use Ctrl-< to stop completion
(ac-config-default)
;; A note about ac-auto-start:
;; If it's set to something like 3 or 4, it is ignored
;; when other sources like ac-math or jedi are used.
;; Set it to nil to avoid autocomplete coming up automatically
;; For yasnippets use it is better to set it to nil
;; and call ac with TAB.
;(when window-system
;  (set-face-font 'ac-candidate-face "Consolas-14")
;  (set-face-font 'ac-selection-face "Consolas-14")
;  (set-face-font 'popup-tip-face "Consolas-13")
;  (set-face-background 'popup-tip-face "#050f13")
;  (set-face-foreground 'popup-tip-face "#ff7400")
;  )

;; ac-ispell, complete words
;; Completion words longer than 4 characters
(setq ac-ispell-requires 4
      ac-ispell-fuzzy-limit 0)
(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))
(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(ac-flyspell-workaround)

(after 'linum
       (ac-linum-workaround))

(after 'yasnippet
       (add-hook 'yas-before-expand-snippet-hook (lambda () (auto-complete-mode -1)))
       (add-hook 'yas-after-exit-snippet-hook (lambda () (auto-complete-mode t)))
       (defadvice ac-expand (before advice-for-ac-expand activate)
         (when (yas-expand)
           (ac-stop))))

(require-package 'ac-etags)
(setq ac-etags-requires 1)
(after 'etags
       (ac-etags-setup))
;;; ----------------------------------------------------------------------

(provide 'init-autocomplete)
