;; ----- Python -----;;;
(require-package 'jedi)
(require-package 'elpy)
(require-package 'flymake-cursor)
;; Python Jedi support
;(setq jedi:complete-on-dot t)                 ; optional
; (setq jedi:tooltip-method nil)                ; optional
;(set-face-attribute 'jedi:highlight-function-argument nil
;                    :foreground "green")
;; (setq jedi:setup-keys t)                      ; optional
; (add-hook 'python-mode-hook 'jedi:setup)
;; Set up Jedi-direx
;(eval-after-load "python"
;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;(add-hook 'jedi-mode-hook 'jedi-direx:setup)
;; Elpy for python development
(elpy-enable)
(elpy-use-ipython)
;; By default elpy uses rope. I prefer jedi but it loads
;; auto-complete automatically.
; (setq elpy-rpc-backend "jedi")
; flymake cursor is used to display the error in the minibuffer
; (require 'flymake-cursor)
; According to the wiki, help-at-pt provides similar functionality and is built-in
(custom-set-variables
 '(help-at-pt-timer-delay 0.9)
 '(help-at-pt-display-when-idle '(flymake-overlay)))
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
;; ----- End python ;;;

(provide 'init-python)
