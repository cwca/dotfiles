;;;
;;; Manually run this with M-x eval-buffer to refresh local Packages
;;;

;; Usage - restore and update packages manually
;;
(package-refresh-contents)

(package-install 'markdown-mode)
(package-install 'auto-complete)
(package-install 'git-gutter)
(package-install 'ac-slime)   ;; auto-complete for Superior Lisp Enviornment (SLIME)
(package-install 'swift-mode)
(package-install 'go-mode)
(package-install 'julia-mode)
(package-install 'gist)
(package-install 'cmake-mode)
(package-install 'csharp-mode)
(package-install 'rust-mode)
(package-install 'yaml-mode)     
(package-install 'flycheck)   ;; syntax checking
(package-install 'paredit)    ;; structured editing of S-expression data (Lisp/Scheme)
(package-install 'python-mode)
(package-install 'elpy)
(package-install 'solarized-theme)
(package-install 'graphviz-dot-mode)
(package-install 'web-mode)

;; Other packages used in the past, but not currently.
;; (package-install 'htmlize)    ;; convert buffer and text decorations to HTML
;; (package-install 'lua-mode)
;; (package-install 'magit)      ;; git interface
;; (package-install 'org))
;; (package-install 'writegood-mode)  ;; Writing checks - Weasel Words, Passive Voice, Duplicates
;;
;; Notes:
;;   1.  Delete ~/.emacs.d/elpa if the installed package is outdated.
;;   2.  'package-install' modifies .emacs automatically.
;;       Revert its change and set 'package-selected-packages' in .emacs manually.
