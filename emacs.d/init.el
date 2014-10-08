;; Future init.el
;; Try to create a better configuration using modular init files for emacs
;; .emacs is becoming difficult to navigate

(defgroup jp nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom jp-cache-directory (concat user-emacs-directory ".cache/")
  "The storage location for various persistent files."
  :group 'jp)

(add-to-list 'load-path (concat user-emacs-directory "config"))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(require 'cl)
(require 'init-packages)
(require 'init-util)

(defcustom jp-modules
  '(init-core

    init-org
    init-eyecandy
    ;init-macosx

    init-smartparens

    init-yasnippet
    ;init-autocomplete
    init-company

    init-vcs
    ;init-flycheck

    init-ido

    init-python
    init-latex
    init-markdown

    init-misc
    init-evil
    init-bindings
    ; etc...
    )
  "Modules enabled for this configuration."
  :group 'jp)

(dolist (module jp-modules)
  (require module))

;; When Mac, load the mac especific init
(when (eq system-type 'darwin)
  (require 'init-macosx)
  (message "Loaded configuration for Mac OS X"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "56cb99174fad69feba8edd6663c592e77d77332fb4c4bb5be058ef459a426463" default)))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
