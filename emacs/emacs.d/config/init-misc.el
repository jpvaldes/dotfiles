;;; init-misc.el --- misc configuration

;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------
;;; Ace-jump-mode
;;;
(require-package 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;;; 
;;; enable a more powerful jump back function from ace jump mode
;;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; Expand region
;;
(require-package 'expand-region)
(require 'expand-region)
;;----------------------------------------------------------------------

(require-package 'multiple-cursors)
(setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
(after 'evil
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))

(require-package 'wgrep)

(provide 'init-misc)
