;;; init-macosx.el --- fixes and quirks for macos

;;; Description:

;;; Code:

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

(provide 'init-macosx)
