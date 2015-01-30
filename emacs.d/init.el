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
;; When Mac, load the mac especific init
(when (eq system-type 'darwin)
  (require 'init-macosx)
  (message "Loaded configuration for Mac OS X")
  (require 'init-proxy)
  (message "Proxy setup"))
(when (eq system-type 'gnu/linux)
  (require 'init-proxy)
  (message "Proxy setup"))
;; Integration with Cygwin in Windows
(when (eq system-type 'windows-nt)
  (require 'init-cygwin)
  (message "Cygwin path added to Emacs."))

(defcustom jp-modules
  '(init-core

    init-org
    init-eyecandy

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

    init-ispell
    init-flyspell
    ; etc...
    )
  "Modules enabled for this configuration."
  :group 'jp)

(dolist (module jp-modules)
  (require module))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "b7d8113de2f7d9a3cf42335d8eed8415b5a417e7f6382e59076f9f4ae4fa4cee" default)))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
