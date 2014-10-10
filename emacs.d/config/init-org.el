;;; init-org.el --- org configuration

;;; Commentary:

;;; Code:

(require-package 'org)

(add-hook
 'org-load-hook
 (lambda ()
   (setq org-completion-use-ido t)
   (setq org-startup-indented t)
   (setq org-indent-indentation-per-level 4)
   ;; Beautify code blocks in org mode
   ;; fontify code in code blocks
   (setq org-src-fontify-natively t
         org-src-tab-acts-natively t)
   ;; Use pretty entities in org
   (setq org-pretty-entities t
         org-pretty-entities-include-sub-superscripts nil ;; but not super- & subscripts, they are annoying
         org-export-with-sub-superscripts "{}") ; use braces while exporting sub & superscripts.

   (after 'evil
          (add-hook 'org-capture-mode-hook 'evil-insert-state))
   (add-hook 'org-mode-hook (lambda ()
                              (when (or (executable-find "aspell")
                                        (executable-find "ispell")
                                        (executable-find "hunspell"))
                                (flyspell-mode)))
             )
   )
 )

;; Ox-reveal: org exporter for reveal.js
;; Cool presentations with org-mode
;; Path to reveal.js
(require-package 'ox-reveal)
(require 'ox-reveal)
(setq org-reveal-root "file:///usr/local/lib/reveal.js-2.6.2")

;;; The different exporters for org-mode are organized in different libraries
;;; that are not necessarily loaded at startup. One of those libraries
;;; corresponds to Markdown. To load automatically the MD exporter
;;; with Org-mode:
(require 'ox-md)
(require 'ox-beamer)

;;; Org and programming languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (sh . t)
    (emacs-lisp . t)))
(setq org-confirm-babel-evaluate nil)

;; Include the latex-exporter
(require 'ox-latex)
;; Make sure cdlatex is installed
(require-package 'cdlatex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Add also booktabs
(add-to-list 'org-latex-packages-alist '("" "booktabs"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(
        ("frame" "lines")
        ("fontsize" "\\small")
      ))
;; Exporting tables to latex
(setq org-latex-table-caption-above nil
      org-latex-tables-booktabs t)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously can be dangerous to activate!
(setq org-latex-pdf-process
      '("latexmk -pdf -shell-escape -gg %o %f"))
;; Set koma class for export
(add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; Use CDLatex mode in org to typeset math more efficiently
;(require-package cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; More org
;; Add an "easy template" to the list of templates. Use in
;; org with <C + <TAB>
(add-to-list 'org-structure-template-alist '("C" "#+begin_comment\n?\n#+end_comment"))
(add-to-list 'org-structure-template-alist '("ab" "#+begin_abstract\n?\n#+end_abstract"))
(add-to-list 'org-structure-template-alist '("sh" "#+begin_src sh\n?\n#+end_src"))
(add-to-list 'org-structure-template-alist '("p" "#+begin_src python\n?\n#end_src"))
;; Disable evil-auto-indent in org-mode
;; as indents when it should not (IMO)
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq evil-auto-indent nil)))

;; Set org-mode agenda
(setq org-agenda-files (list "~/Documents/org/group_meetings.org"))

;; Deft, a quick note taking app
;; here is set up to use org-mode
;(require-package 'deft)
;(require 'deft)
;(setq deft-extension "org")
;(setq deft-text-mode 'org-mode)

(provide 'init-org)
