;;; init.el --- Personal init file

;;; Commentary:

;; set of personal defaults
;; the idea comes from https://github.com/bling/dotemacs

;;; Code:

;;______________________________;;
;; Start server to call emacs
;; from terminal
;;------------------------------;;
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Save backups in .emacs.d
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backup-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ; use versioned backups

;;; Save place where files were opened when I reopen them
(require 'saveplace)
(setq-default save-place t)

;;; Recently opened files
(require 'recentf)
(setq recentf-max-menu-items 30
      recentf-max-saved-items 100
      recentf-exclude
      (append recentf-exclude
              '("/.emacs.d/el-get" "~$" "/.emacs.d/backup-saves"
                "/.emacs.d/elpa" "\\.log$" "\\.pdfsync$"
                "\\.toc$" "\\.aux$"
                ".emacs/24.3"
                "/tmp/" ".el.gz$"
                ".recentf" ".ido.last"))
      )
(setq recentf-save-file (concat user-emacs-directory "recentf"))
(recentf-mode 1)
(run-with-timer 1800 1800 'recentf-save-list)

;;; save minibuffer, search history
(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring)    
      ;savehist-file "~/.emacs.d/savehist"
      savehist-file (concat user-emacs-directory "savehist")
      savehist-autosave-interval 60
      ) 
(setq-default history-length 1000)
(savehist-mode t)

;;; desktop mode (save buffers on exit)
;;; autosave buffers
;(require 'desktop)
;  (desktop-save-mode 1)
;  (defun my-desktop-save ()
;    (interactive)
;;    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;    (if (eq (desktop-owner) (emacs-pid))
;        (desktop-save desktop-dirname)))
;  (add-hook 'auto-save-hook 'my-desktop-save)

;;; ----------------;;;
;;; Some basic setup
;;; ----------------;;;
;
;;; frame size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 140) (height . 70)))

;;; fringe
(fringe-mode 16)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally) ;; side-by-side diffs
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; no extra frames

; Extend the path in Emacs to find aspell just in case
(when (eq system-type 'darwin)
      (add-to-list 'exec-path "/usr/local/bin")
      )

;;; utf-8 lover
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;; color for terminal, I think :\
(ansi-color-for-comint-mode-on)
;
;;; Read .bash_profile
;(setq explicit-bash-args '("--login" "--init-file" "/Users/valdesj/.bash_profile"))
;
;;; Show line number
(line-number-mode 1)
;
;;; Show column number
(column-number-mode 1)

;; Improve performance of diplay engine
(setq redisplay-dont-pause t)

;;; Title bar shows buffer file name and use mouse wheel when graphical system
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t))
;
;;; Stop beeping, please
(setq visible-bell t)
;
;;; No scroll bar. There's a nice % sign in the status line.
;(scroll-bar-mode -1)

;;; No icon bar.
(tool-bar-mode -1)
;
;;; no crazy blinking cursor anymore
(blink-cursor-mode 0)
;
;;; Truncate lines and don't wrap at window edge
(set-default 'truncate-lines t)
;
;;; Don't open new frames when called from Finder, etc.
(setq ns-pop-up-frames nil)

;; From graphene, don't use tabs for indents and use 4 spaces
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)  

;;; Automatically indent programs. Not very sure I'd need the Fortran 90 bit. 
(define-key global-map (kbd "RET") 'newline-and-indent)
(add-hook 'LaTeX-mode-hook '(lambda ()
			      (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'f90-mode-hook (lambda ()
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;; Some commands force you to confirm using yes or no
;; Redefine it to y or no, instead:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set auto-fill for text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; Scrolling
;;; Mighty mouse scrolling very jerky ;;;
;(setq mouse-wheel-scroll-amount '(0.01))
;; Nicer scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

(random t) ;; seed

(provide 'init-core)
