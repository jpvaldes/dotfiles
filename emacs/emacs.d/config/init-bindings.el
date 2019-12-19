;;; init-bindings.el --- Bindings for different modes

;;; Description:

;;; Code:

(require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" ","))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

(after 'evil
       (after "evil-leader-autoloads"
              (evil-leader/set-leader ",")
              (evil-leader/set-key
                "e" 'find-file
                "b" 'switch-to-buffer
                "k" 'kill-buffer
                "l" 'ace-jump-line-mode
                "r" 'recentf-ido-find-file ; or the normal recentf-open-files
                "w" 'save-buffer
                "v" (kbd "C-w v C-w l")
                "s" (kbd "C-w s C-w j")
                "h" help-map
                "h h" 'help-for-help-internal
                "c" 'evil-ex-nohighlight ; resets highlight like :noh 
                )
              (evil-leader/set-key-for-mode 'org-mode
                "-" 'org-cdlatex-math-modify
                "." 'cdlatex-math-symbol
                )
              (setq evil-leader/in-all-states t) ; Do C-<leader> in normal mode
              ;; Custom evil leader for latex mode
                                        ;(evil-leader/set-key-for-mode 'latex-mode
                                        ;  "c" 'TeX-command-master
                                        ;  "v" 'TeX-view)
              )

       (after "evil-numbers-autoloads"
              (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
              (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)
              )

       ;; Manage windows a la vim
       (global-set-key (kbd "C-w") 'evil-window-map)
       (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
       (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
       (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
       (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

       ;; After searching in evil mode, the words stay highlighted
       ;; That can be confusing after a while
       ;; the vim command is :noh and there is and option in
       ;; vim to set it from the start: set nohlsearch
       ;(define-key evil-normal-state-map (kbd "C-ö") 'evil-ex-nohighlight)

       (after "ace-jump-mode-autoloads"
        ;;; you can select the key you prefer to
              (define-key global-map (kbd "C-#") 'ace-jump-mode)
        ;;; 
        ;;; enable a more powerful jump back function from ace jump mode
        ;;;
              (define-key global-map (kbd "C-ä") 'ace-jump-mode-pop-mark)
        ;;;If you use evil
        ;; ace-jump-mode = ace-jump-word-mode -> C-u ace-jump-char-mode
        ;; -> C-u C-u ace-jump-line-mode. See ace jump mode wiki.
              (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
              (define-key evil-normal-state-map (kbd "M-SPC") 'evil-ace-jump-char-mode)
              (define-key evil-motion-state-map (kbd "S-SPC") 'evil-ace-jump-line-mode)
              )
       )

(after 'auto-complete
       (define-key ac-completing-map (kbd "C-n") 'ac-next)
       (define-key ac-completing-map (kbd "C-p") 'ac-previous)
       )

(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)

  ;;; Company with yanippet and indent bound to TAB
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun company-tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        ;; (ido-complete)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  (global-set-key [tab] 'company-tab-indent-or-complete)
  )

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  )

(global-set-key "\C-x\ \C-r" 'recentf-ido-find-file)

;; Remap Yasnippet from TAB to C-TAB
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-TAB") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

(provide 'init-bindings)
