;;; init.el --- Personal init file

;;; Commentary:

;; set of personal defaults

;;; Code:

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "149.203.231.111:4711")
     ("https" . "149.203.231.111:4711")))

;; Emacs repository of packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; ----------------;;;
;;; Some basic setup
;;; ----------------;;;
;
;;; frame size
(setq initial-frame-alist '((top . 0) (left . 0) (width . 140) (height . 70)))
;;; utf-8 lover
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
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

;; Linum mode
(setq linum-format " %4d ") ;; Avoid glitches - Graphene
(global-linum-mode 1)

;; Improve performance of diplay engine
(setq redisplay-dont-pause t)

;; From graphene, don't use tabs for indents and use 4 spaces
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)  

;; Some commands force you to confirm using yes or no
;; Redefine it to y or no, instead:
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set auto-fill for text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; Set a font
(when window-system
    (setq-default line-spacing 2)
    ;(set-face-font 'default "Menlo-14")
    ;(set-face-font 'fixed-pitch "Menlo-14")
    (set-face-font 'default "Source Code Pro Medium-14")
    (set-face-font 'fixed-pitch "Source Code Pro Medium-14")
    (set-frame-font "Source Code Pro")
    (set-face-font 'variable-pitch "Lucida Grande-14")
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

;
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
;
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

;;; Automatically indent programs. Not very sure I'd need the Fortran 90 bit. 
(define-key global-map (kbd "RET") 'newline-and-indent)
(add-hook 'LaTeX-mode-hook '(lambda ()
			      (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'f90-mode-hook (lambda ()
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;;;-------------------- IDO ------------------------------;;;
;;;http://emacswiki.org/emacs/InteractivelyDoThings 
;(require 'ido) ; I think this isn't needed anymore
;http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
(ido-mode 1)
(setq ido-enable-flex-matching t) ; Enable fuzzy matching a la Ctrl-P in vim
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order
      '(".tex" ".txt" ".org" ".py" ".emacs" ".html" ".css" ".bib" ".el" ".cfg"))
(setq ido-ignore-extensions t) ; Ignore completion-ignored-extensions
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
;
;;;-------------------- END IDO -------------------------;;;
(icomplete-mode t) ; Enable minibuffer completion

;;;-------------------- Ibuffer -------------------------;;;
(defalias 'list-buffers 'ibuffer)
;;;------------------------------------------------------;;;

;; Save backups in .emacs.d
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "/Users/valdesj/.emacs.d/backup-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ; use versioned backups
;;; Save place where files were opened when I reopen them
(require 'saveplace)
(setq-default save-place t)
;;; Recently opened files
(require 'recentf)
(setq recentf-max-menu-items 15
      recentf-max-saved-items 50
      recentf-exclude
      (append recentf-exclude
	      '("/.emacs.d/el-get" "~$" "/.emacs.d/backup-saves"
        "/.emacs.d/elpa" "\\.log$" "\\.pdfsync$"
        "\\.toc$" "\\.aux$"
        ".emacs/24.3"
        "/tmp/" ".el.gz$"))
      )
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;;; save minibuffer, search history
(require 'savehist)
(setq savehist-additional-variables        ; more stuff to save...
      '(search-ring regexp-search-ring)    ; ...include also searches
      savehist-file "~/.emacs.d/savehist") ; keep $HOME clean
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

;;;-------------------- EVIL--------------------;;;

;; Enable evil-leader before evil-mode as per instructions.
(require 'evil-leader)
(global-evil-leader-mode)

; Put setq customizations before loading evil as
; suggested in the manual
; Cursor:
(setq evil-normal-state-cursor '("OliveDrab3" box))
(setq evil-insert-state-cursor '("red" hbar))
(setq evil-visual-state-cursor '("yellow" hollow))
;; State tags
(setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "red" :foreground "black")))
      evil-visual-state-tag (propertize "V" 'face '((:background "yellow1" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "blue")))
      evil-motion-state-tag (propertize "M" 'face '((:background "purple"))))
;; Cursor moves backwards when exiting insert mode
;; I found this irritating
(setq evil-move-cursor-back nil)
;; In order to change cursor color while using evil
;;(setq evil-default-cursor t)
;;; be eVIl!! http://gitorious.org/evil
(require 'evil)
(evil-mode 1)
;;; Even more evil, use evil-surround, the evil version of surround.vim.
(require 'evil-surround)
(global-evil-surround-mode 1)
;;;;-------------------- EVIL CONFIG END --------------------;;;

;;;----- EVIL LEADER CONFIG BEGIN --------------------------;;;
;; WARNING: I don't know why but the setup of evil-leader must have
;; the require before evil's own require and the options set
;; afterwards in this order.
;;
;; Change leader key from default \ to º.
(evil-leader/set-leader "ß")
(evil-leader/set-key
 "e" 'find-file
 "b" 'switch-to-buffer
 "k" 'kill-buffer
 "l" 'ace-jump-line-mode
 "r" 'recentf-ido-find-file ; or the normal recentf-open-files
 )
(evil-leader/set-key-for-mode 'org-mode
 "," 'org-cdlatex-math-modify
 "." 'cdlatex-math-symbol
  )
(setq evil-leader/in-all-states t) ; Do C-<leader> in normal mode
;; Custom evil leader for latex mode
;(evil-leader/set-key-for-mode 'latex-mode
;  "c" 'TeX-command-master
;  "v" 'TeX-view)
;; Enable evil-leader globally.
;(global-evil-leader-mode)
;;;----- EVIL LEADER CONFIG END ---------------------------;;;

;; ----- Python -----;;;
;; Python Jedi support
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
;(setq jedi:tooltip-method nil)                ; optional
;; (setq jedi:setup-keys t)                      ; optional
;; Set up Jedi-direx
;(eval-after-load "python"
;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;(add-hook 'jedi-mode-hook 'jedi-direx:setup)
;; Elpy for python development
(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")
;; Changes to flymake as it is used by default
;; by elpy
;; Wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)
;; Show error in minibufer instead of tooltip
(require 'flymake-cursor)
;;set ipython as default python shell
;(setq
; python-shell-interpreter "ipython"
; python-shell-interpreter-args ""
; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
; python-shell-completion-setup-code
;   "from IPython.core.completerlib import module_completion"
; python-shell-completion-module-string-code
;   "';'.join(module_completion('''%s'''))\n"
; python-shell-completion-string-code
;   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Flycheck. Better Flymake. Doesn't work well on windows yet.
;(add-hook 'after-init-hook #'global-flycheck-mode)
;; ----- End python ;;;

;;; ----------------------------------------------------------------------
;;; Autocomplete
;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(setq ac-auto-show-menu t)
;;; It wasn't working on Mac OS X
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")
(global-auto-complete-mode t)
(ac-config-default)
(setq ac-auto-show-menu 0.25 ; or t for instant
      ac-auto-start 3 ; wait for 3 characters to start, otherwise t
      ac-delay 0.125
      ac-ignore-case nil
      ac-candidate-menu-min 2
      ac-limit 10
      ac-use-quick-help t
      ac-quick-help-delay 0.5
      ac-show-menu-immediately-on-auto-complete t
      ac-use-fuzzy t
      ac-fuzzy-cursor-color "magenta4"
      ac-menu-height 6)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-completing-map "C-<" 'ac-stop) ; use Ctrl-< to stop completion
;; A note about ac-auto-start:
;; If it's set to something like 3 or 4, it is ignored
;; when other sources like ac-math or jedi are used.
;; Set it to nil to avoid autocomplete coming up automatically
;; For yasnippets use it is better to set it to nil
;; and call ac with TAB.
(when window-system
  (set-face-font 'ac-candidate-face "Consolas-14")
  (set-face-font 'ac-selection-face "Consolas-14")
  (set-face-font 'popup-tip-face "Consolas-13")
  (set-face-background 'popup-tip-face "#050f13")
  (set-face-foreground 'popup-tip-face "#ff7400")
  )
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
;;; ----------------------------------------------------------------------

;;;----------------------------------------;;;
;;; Yasnippets
;;;----------------------------------------;;;
;(add-to-list 'yas/root-directory "/Users/valdesj/.emacs.d/yasnippet-snippets")
(require 'yasnippet)
(setq yas-snippet-dirs '("/Users/valdesj/.emacs.d/yasnippet-snippets"))
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-TAB") 'yas/expand)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas/expand)
(yas-global-mode 1)

;;----------------------------------------------------------------------
;;; Ace-jump-mode
;;;
;;; Bound to C-#
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;;; you can select the key you prefer to
(define-key global-map (kbd "C-#") 'ace-jump-mode)
;;; 
;;; enable a more powerful jump back function from ace jump mode
;;;
;;; Bound to C-ß
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-ä") 'ace-jump-mode-pop-mark)
;;;If you use evil
;; ace-jump-mode = ace-jump-word-mode -> C-u ace-jump-char-mode
;; -> C-u C-u ace-jump-line-mode. See ace jump mode wiki.
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "M-SPC") 'ace-jump-char-mode)
;;----------------------------------------------------------------------

;;------------------------------------------------------------
;; Autopair (make need workaround in latex buffers)
;;
;(require 'autopair)
;(autopair-global-mode t) ;; enable autopair in all buffers
;;; Let me write triple quotes in python
;(add-hook 'python-mode-hook
;          #'(lambda ()
;              (setq autopair-handle-action-fns
;                    (list #'autopair-default-handle-action
;                          #'autopair-python-triple-quote-action))))
;;; Latex paired-delimiter helper
;(add-hook 'LaTeX-mode-hook
;          #'(lambda ()
;              (setq autopair-handle-action-fns
;                    (list #'autopair-default-handle-action
;                          #'autopair-latex-mode-paired-delimiter-action))))
;;; Define some extra pairs
;;; It is possible to define where they go...
;;; :everywhere, :code, :string, :comment
;;; Python and []. Actually not needed. Only example.
;(add-hook 'python-mode-hook
;          #'(lambda ()
;              (push '(?[ . ?])
;                    (getf autopair-extra-pairs :everywhere))))
;;------------------------------------------------------------

;;------------------------------------------------------------;;
;;          Smartparens
;;------------------------------------------------------------;;
;; Try the alternative to autopair
(smartparens-global-mode 1)
;; Stopping from completing ' in elisp, which is annoying
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;;------------------------------------------------------------;;

;;----------------------------------------------------------------------
;; Expand region
;;
(require 'expand-region)
(global-set-key (kbd "C-ä") 'er/expand-region)
;;----------------------------------------------------------------------

;; theme
(load-theme 'monokai t)
;;; highlight the current line; set a custom face, so we can
;;; recognize from the normal marking (selection)
(global-hl-line-mode t) ; turn it on for all modes by default
;(set-face-background 'hl-line "firebrick4")
;(set-face-background 'hl-line "#751a1a")
;(set-face-background 'hl-line "#3e4446")
;(set-face-background 'hl-line "#383736")
(set-face-background 'hl-line "#3e1146")
(set-face-foreground 'highlight nil) ; keep syntax highlighting
(set-face-attribute 'region nil
                    :background "#888")

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
      ispell-personal-dictionary "~/Library/Spelling/personal-dict")
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
;;;----------------------------------------;;;
;;; More setup
;;;----------------------------------------;;;

;; Markdown and Pandoc export
(setq markdown-command "pandoc --smart --from=markdown --to=html" )
; (setq-markdown-css-path (expand-file-name "markdown.css" jose/cssdir))

;; Ox-reveal: org exporter for reveal.js
;; Cool presentations with org-mode
;; Path to reveal.js
(require 'ox-reveal)
(setq org-reveal-root "file:///usr/local/lib/reveal.js-2.6.2")

;;; The different exporters for org-mode are organized in different libraries
;;; that are not necessarily loaded at startup. One of those libraries
;;; corresponds to Markdown. To load automatically the MD exporter
;;; with Org-mode:
(require 'ox-md)
(require 'ox-beamer)

;;; Org and programming languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (sh . t)
    (emacs-lisp . t)))
(setq org-confirm-babel-evaluate nil)

;;; Beautify code blocks in org mode
;; fontify code in code blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)
;; Use pretty entities in org
(setq org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil ;; but not super- & subscripts, they are annoying
      org-export-with-sub-superscripts "{}") ; use braces while exporting sub & superscripts.

;; Include the latex-exporter
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Add also booktabs
(add-to-list 'org-latex-packages-alist '("" "booktabs"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(
        ("frame" "lines")
        ("fontsize" "\\small")
      ))
;; Exporting tables to latex
(setq org-latex-table-caption-above nil
      org-latex-tables-booktabs t)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously can be dangerous to activate!
(setq org-latex-pdf-process
      '("latexmk -pdf -shell-escape -gg %o %f"))
;; Set koma class for export
(add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Use CDLatex mode in org to typeset math more efficiently
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; More org
;; Add an "easy template" to the list of templates. Use in
;; org with <C + <TAB>
(add-to-list 'org-structure-template-alist '("C" "#+begin_comment\n?\n#+end_comment"))
(add-to-list 'org-structure-template-alist '("ab" "#+begin_abstract\n?\n#+end_abstract"))
;; Disable evil-auto-indent in org-mode
;; as indents when it should not (IMO)
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq evil-auto-indent nil)))

;; Set org-mode agenda
(setq org-agenda-files (list "~/Documents/org/group_meetings.org"))

;; Deft, a quick note taking app
;; here is set up to use org-mode
;(require 'deft)
;(setq deft-extension "org")
;(setq deft-text-mode 'org-mode)

; Alt doesn't work in Mac OS X to type other characters
; Tell emacs to delete with the delete key and that the left-Alt
; is meta while the right is a modifier (AltGr)
(if (eq system-type 'darwin)
    (progn
      ;; "fix" the keyboard
      ;(global-set-key "\M-5" '(lambda () (interactive) (insert "[")))
      ;(global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
      ;(global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
      ;(global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
      ;(global-set-key "\M-8" '(lambda () (interactive) (insert "{")))
      ;(global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
      ;(global-set-key "\M-n" '(lambda () (interactive) (insert "~")))
      ;(global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
      (global-set-key [kp-delete] 'delete-char)
      ))
(setq ns-right-alternate-modifier nil)

;;; Mac OS X fix for the python console
(setenv "LC_CTYPE" "de_DE.UTF-8")

;;------------------------------------------------------------;;
;; Powerline
;;------------------------------------------------------------;;
(require 'powerline)
(powerline-evil-center-color-theme)

;;; More fixes for Mac OS X ;;;
;;; exec-path-from-shell    ;;;
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;;; no graphical popup dialogs ;;;
;;; http://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
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
;;; From graphene
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Emacs window manager E2WM
;(require 'e2wm)
;(global-set-key (kbd "M-+") 'e2wm:start-management)

;;______________________________;;
;; Start server to call emacs
;; from terminal
;;------------------------------;;
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------;;
;;                              Guide key
;;----------------------------------------------------------------------;;
;(require 'guide-key)
;(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
;(guide-key-mode 1) ; enable the mode

(provide 'init.el)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((py-indent-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
