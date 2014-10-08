;;; init-ido.el --- ido mode configuration

;;; Commentary:

;;; Code:

;;;-------------------- IDO ------------------------------;;;
;;;http://emacswiki.org/emacs/InteractivelyDoThings 
;(require 'ido) ; I think this isn't needed anymore
;http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
(require 'ido)

(setq ido-use-virtual-buffers t)
(setq ido-enable-flex-matching t) ; Enable fuzzy matching a la Ctrl-P in vim
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer t)
(setq ido-file-extensions-order
      '(".tex" ".txt" ".org" ".py" ".emacs" ".html" ".css" ".bib" ".el" ".cfg"))
(setq ido-ignore-extensions t) ; Ignore completion-ignored-extensions

(ido-mode t)
(ido-everywhere t)
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
;
;;;-------------------- END IDO -------------------------;;;

(provide 'init-ido)
