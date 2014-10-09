;;; init-company.el --- Company mode config

;;; Description:

;;; First version totally copied from bling/dotemacs

;;; Code:

(require-package 'company)
(require 'company)

(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 3)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)
(setq company-auto-complete nil)

(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(setq company-backends (remove 'company-ropemacs company-backends))
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-ispell))))

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)

(set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
(set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
(set-face-attribute 'company-preview nil :background "black")
(set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
(set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
(set-face-attribute 'company-scrollbar-fg nil :background "gray40")

(when (executable-find "tern")
  (after "company-tern-autoloads"
    (add-to-list 'company-backends 'company-tern)))

(setq company-global-modes
      '(not
        eshell-mode comint-mode))

(add-hook 'after-init-hook 'global-company-mode)

(defadvice company-complete-common (around advice-for-company-complete-common activate)
  (when (null (yas-expand))
    ad-do-it))

(provide 'init-company)
