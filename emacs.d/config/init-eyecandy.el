;;; init-eyecandy.el --- Visual configuration

;;; Commentary:

;;; Code:

(show-paren-mode)
(setq show-paren-delay 0)


(line-number-mode t)
(column-number-mode t)
(display-time-mode t)
(size-indication-mode t)

(require-package 'diminish)
(diminish 'visual-line-mode)
(after 'autopair (diminish 'autopair-mode))
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'auto-complete (diminish 'auto-complete-mode))
(after 'projectile (diminish 'projectile-mode))
; (after 'yasnippet (diminish 'yas-minor-mode))
(after 'guide-key (diminish 'guide-key-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'smartparens (diminish 'smartparens-mode))
(after 'company (diminish 'company-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'git-gutter (diminish 'git-gutter-mode))
; (after 'magit (diminish 'magit-auto-revert-mode))

;;; Set a font
(when window-system
  (when (eq system-type 'darwin)
    (setq-default line-spacing 2)
    ;(set-face-font 'default "Menlo-14")
    ;(set-face-font 'fixed-pitch "Menlo-14")
    (set-face-font 'default "Source Code Pro Medium-14")
    (set-face-font 'fixed-pitch "Source Code Pro Medium-14")
    (set-frame-font "Source Code Pro")
    (set-face-font 'variable-pitch "Lucida Grande-14")
    )
  (when (eq system-type 'windows-nt)
      (set-face-font 'default "Consolas-12")
      (set-face-font 'fixed-pitch "Consolas-12")
      (set-frame-font "Consolas")
      (set-face-font 'variable-pitch "Verdana-12")
    )
  )
;  (set-frame-font "Menlo")
;  (set-face-attribute 'default nil :font "Menlo" :height 150)
;  (set-face-font 'default "Menlo")

;(set-default-font "Anonymous Pro-11")
;(set-frame-font "Source Code Pro")
;(set-face-attribute 'default nil :font "Source Code Pro" :height 100)
;(set-face-font 'default "Source Code Pro")
;(set-default-font "Dejavu Sans Mono-10")
;(set-default-font "Source Code Pro-10")

;; theme - monokai not working as of 191114
; (require-package 'monokai-theme)
; (load-theme 'monokai t)
(load-theme 'misterioso t)
(set-face-attribute 'region nil
                    :background "gray39")
;;; highlight the current line; set a custom face, so we can
;;; recognize from the normal marking (selection)
;(set-face-background 'hl-line "firebrick4")
;(set-face-background 'hl-line "#751a1a")
;(set-face-background 'hl-line "#3e4446")
;(set-face-background 'hl-line "#383736")
;(set-face-background 'hl-line "#3e1146")
;(set-face-background 'hl-line "purple4")
;; Change the background colour of the selected region
(global-hl-line-mode +1) ; turn it on for all modes by default
(set-face-background hl-line-face "DarkSlateGray")
(set-face-foreground 'highlight nil) ; keep syntax highlighting

;; Show matching parentheses
(setq show-paren-delay 0) ; how long to wait?
(show-paren-mode t) ; turn paren-mode on
(setq show-paren-style 'parenthesis) ; alternatives are expression and mixed
(set-face-background 'show-paren-match-face "black") ; Background Colour
(set-face-foreground 'show-paren-match-face "yellow2") ; Foreground colour
(set-face-attribute 'show-paren-match-face nil :weight 'bold
		    :underline nil :overline nil :slant 'normal) ; font attributes
(set-face-background 'show-paren-mismatch-face "orange") ; Color for mismatch
(set-face-attribute 'show-paren-mismatch-face nil
		    :weight 'bold :underline t :overline nil :slant 'normal) ; font attributes for a mismatch

;;------------------------------------------------------------;;
;; Powerline
;;------------------------------------------------------------;;
(require-package 'powerline)
(require-package 'powerline-evil)
(require 'powerline)
(powerline-evil-center-color-theme)

;(require-package 'color-identifiers-mode)
;(global-color-identifiers-mode)
;(diminish 'color-identifiers-mode)

(require-package 'idle-highlight-mode)
(setq idle-highlight-idle-time 0.5)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

;(require-package 'rainbow-delimiters)
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'find-file-hook 'hl-line-mode)

;; Linum mode
;(setq linum-format "%4d  ") ;; Avoid glitches - Graphene
(setq linum-format "%d")
;(set-face-attribute 'linum nil :height 130)
(global-linum-mode 1)

(provide 'init-eyecandy)
