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
