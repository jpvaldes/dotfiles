;;; init-cygwin.el --- cygwin integration on windows

;;; Commentary:

;;; Code:

(require-package 'cygwin-mount)

(defvar cygwin-bin-dir
  "c:/Cygwin64/bin/"
  "*Directory of Cygwin bin.")

(setenv "PATH" (concat cygwin-bin-dir ";" (getenv "PATH")))
(setq exec-path (cons cygwin-bin-dir exec-path))
(require 'cygwin-mount)
(cygwin-mount-activate)

(provide 'init-cygwin)
