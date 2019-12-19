;;; init-flymake.el --- Flymake mode configuration

;;; Commentary:

;;; Code:

;; Changes to flymake as it is used by default
;; by elpy
;; Wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)
;; Show error in minibufer instead of tooltip
(require 'flymake-cursor)

(provide 'init-flymake)

