
;;;
;;; Packages
;;;

(if (> emacs-major-version 24)
    (progn
      (require 'package)
      (package-initialize)
      (setq package-check-signature nil)  ; workaround public key failure
      (setq package-archives
            '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
              ("MELPA Stable" . "https://stable.melpa.org/packages/")
              ("MELPA"        . "https://melpa.org/packages/"))
            package-archive-priorities
            '(("MELPA Stable" . 10)
              ("GNU ELPA"     . 5)
              ("MELPA"        . 0)))
      ;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
      ;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
      ))

;;;
;;; Environments
;;;

(set-language-environment 'UTF-8)

;;;
;;; Basic Settings
;;;

;; Disable backup and autosave
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)
(setq backup-directory-alist `((".*(" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Disable startup messages
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; allow y/n instead of yes/no on prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; show parenthesis
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(electric-pair-mode 1)

;; truncate lines, don't wrap.
(set-default 'truncate-lines t)

;; Narrow-to-region C-x n n and C-x n w
(put 'narrow-to-region 'disabled nil)

;; Make column count start at 1 instead of zero
(setq column-number-indicator-zero-based nil)

;; misc
(display-time)
(delete-selection-mode t)
(transient-mark-mode t)
(setq column-number-mode t)
(setq visible-bell t)
(setq echo-keystrokes 0.1)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(setq use-dialog-box nil)

(setq x-select-enable-clipboard t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq fill-column 79)

;; Disable scrollbar, menubar, toolbar
(if window-system
    (set-scroll-bar-mode nil)
  (setq scroll-bar-mode nil))
(menu-bar-mode -1)
(if window-system
    (tool-bar-mode -1))

;; Indentation
(setq tab-width 4)
(setq-default indent-tabs-mode nil) ;; don't indent with tab

;;;
;;; Programming Major Modes
;;;

;;; [C Mode]

;; Applies to C, C++, Objective-C, and Java
(add-hook 'c-mode-common-hook
          '(lambda ()
             (subword-mode 1)  ; Google C++ style uses MixedCase
             (outline-minor-mode)
             (setq show-trailing-whitespace t)
             (c-toggle-auto-hungry-state -1)
             (c-toggle-auto-newline -1)
             (c-toggle-electric-state t)
             (c-set-offset 'case-label '+)
             ;; ff-find-other-file will read #include lines, use
             ;; ff-get-other-file instead.
             (local-set-key  (kbd "M-h") 'ff-get-other-file) ;; be compatible with ST3
             (setq c-tab-always-indent t)
	     (setq tab-width 2)  ; Google C++ Style
             (setq c-basic-offset 2)
             ;; C++ style
             (setq comment-start "//" comment-end "")
             (local-set-key [(return)] 'newline-and-indent)
             (set (make-local-variable 'cc-other-file-alist)
                  '(("\\.m\\'" (".h"))
                    ("\\.mm\\'" (".h"))
                    ("\\.h\\'" (".m" ".mm" ".c" ".cpp" ".cc"))
                    ("\\.c\\'" (".h"))
                    ("\\.cpp\\'" (".h" ".hxx" ".hpp"))
                    ("\\.cc\\'" (".h" ".hxx" ".hpp"))
                    ("\\.cxx\\'" (".h" ".hxx"))
                    ("\\.hxx\\'" (".cc" ".cxx" ".cpp"))
                    ("\\.hpp\\'" (".cc" ".cxx" ".cpp"))
                    ))))

;; Set style for special projects.
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "linux")
             (subword-mode 0)
             (setq comment-start "/*" comment-end "*/")
             (setq indent-tabs-mode nil)))

;;; [Python Mode]

(add-hook 'python-mode-hook
          (lambda ()
            (outline-minor-mode)
            (subword-mode)  ; Python uses MixedCase
            (setq show-trailing-whitespace t)
            (setq tab-width 2)  ; python-mode overrides tab-width to 8
            (setq-local fill-column 79)
            (setq python-indent-offset 4)))

;;; [Java Mode]

(add-hook 'java-mode-hook
          (lambda ()
            (setq-local fill-column 100)))


;;; [Obj-C Mode]

(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

(add-hook 'objc-mode-hook
          '(lambda ()
             (subword-mode 1)  ; Obj-C uses MixedCase.
             ;;(setq truncate-lines nil)
             ))

;;; [Swift Mode]

(when (require 'swift-mode nil 'noerror)
  (add-hook 'swift-mode-hook
            '(lambda ()
	       (setq tab-width 4)
               (subword-mode 1)
               )))


;;; [Go Mode]

;; See http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
(when (require 'go-mode nil 'noerror)
  (progn
    (if (> emacs-major-version 24)
        ;; It's standard practice to run gofmt when saving.
        (add-hook 'before-save-hook 'gofmt-before-save))
    (add-hook 'go-mode-hook
              '(lambda ()
                 (subword-mode 1)  ; Go uses MixedCase.
                 ))))

;;; [CUDA - C++ Mode]

(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))


;;; [GN - Python Mode]

(add-to-list 'auto-mode-alist '("\\.gn$" . python-mode))

;;; [JavaScript - JS2 Mode]

(when (require 'js2-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook
            (lambda ()
            ;; Scan the file for nested code blocks
            (setq js2-basic-offset 2)
            (subword-mode 1)
            (setq truncate-lines nil))))


;;; [HTML - Web Mode]

(when (require 'web-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-hook 'web-mode-hook
          '(lambda ()
             (setq truncate-lines nil)
             (setq web-mode-enable-auto-indentation nil))))

;;; [CSS Mode]

;; Support .less file.
(when (require 'css-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.less$" . css-mode)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 4)))

;;; [SGML Mode]

(require 'sgml-mode)
(define-key sgml-mode-map "\\r" 'newline-and-ident)


;;;
;;; Non-Programming Major Modes
;;;

;;; [Outline Mode]

(add-hook 'outline-mode-hook '(lambda ()
                                ;; Collapse subnodes when opening outline file
                                (hide-sublevels 1)))
;;; [Flyspell Mode]

(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
	(setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")


;;; [Markdown Mode]

(when (require 'markdown-mode nil t)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (flyspell-mode)
              (outline-minor-mode)
              (setq truncate-lines nil)
              ;; Markdown mode override these keys. Set it back.
              (local-set-key "\M-p" '(lambda () (interactive) (previous-line 3)))
              (local-set-key "\M-n" '(lambda () (interactive) (next-line 3)))
              )))

;;; [Rebase Mode]

;; Disable git-rebase mode.
(setq auto-mode-alist (delete '("git-rebase-todo" . rebase-mode)
                              auto-mode-alist))

;;; [Dired Mode]

(require 'dired-x)

;; Use C-x M-o to toggle.
(setq-default dired-omit-mode t) ; this is buffer-local variable

;; Ignore dot files and system files on Mac.
(setq dired-omit-files
      (concat dired-omit-files
              "\\|^\\..+$\\|desktop.ini\\|__pycache__\\|.DS_Store\\|.git\\|Thumbs.db\\|Icon\015"))

;;; [Occur Mode]

;; Bind p, n keys to be consistent with grep mode.
(add-hook 'occur-mode-hook
          '(lambda ()
             (define-key occur-mode-map "p" '(lambda () (interactive)
                                               (occur-prev)
                                               (occur-mode-goto-occurrence-other-window)
                                               (other-window 1)))
             (define-key occur-mode-map "n"  '(lambda () (interactive)
                                                (occur-next)
                                                (occur-mode-goto-occurrence-other-window)
                                                (other-window 1)))))

;; Jig Mode

;;
;; Extending prefix keys for Jig
;;

(define-key 'iso-transl-ctl-x-8-map "f"  [?ƒ])
(define-key 'iso-transl-ctl-x-8-map "("  [?‹])
(define-key 'iso-transl-ctl-x-8-map ")"  [?›])
(define-key 'iso-transl-ctl-x-8-map ";"  [?←])
(define-key 'iso-transl-ctl-x-8-map ":"  [?→])
(define-key 'iso-transl-ctl-x-8-map "<"  [?≤])
(define-key 'iso-transl-ctl-x-8-map ">"  [?≥])
(define-key 'iso-transl-ctl-x-8-map "="  [?≠])
(define-key 'iso-transl-ctl-x-8-map "\\" [?«])
(define-key 'iso-transl-ctl-x-8-map "|"  [?»])
(define-key 'iso-transl-ctl-x-8-map "r"  [?…])
(define-key 'iso-transl-ctl-x-8-map "e"  [?∈])  ;; ELEMENT OF
(define-key 'iso-transl-ctl-x-8-map "E"  [?∉])  ;; NOT AN ELEMENT OF
(define-key 'iso-transl-ctl-x-8-map "p"  [?¤])
(define-key 'iso-transl-ctl-x-8-map "s"  [?§])
(define-key 'iso-transl-ctl-x-8-map "t"  [?×])  ;; MULTIPLY
(define-key 'iso-transl-ctl-x-8-map "u"  [?↑])
(define-key 'iso-transl-ctl-x-8-map "d"  [?↓])
(define-key 'iso-transl-ctl-x-8-map "l"  [?λ])
(define-key 'iso-transl-ctl-x-8-map "n"  [?⍝])


;; Change C-x o to 'switch-window'

(global-set-key (kbd "C-x o") 'switch-window)

;;; [Hexl Mode]

(setq hexl-bits 8)


;;;
;;; Minor Modes
;;;

;;; [Auto Complete Mode]

;; Enable global auto-complete mode
;;(if (> emacs-major-version 24)  ; be compatible with old emacs
;;    (progn
;;      (setq package-selected-packages (quote (auto-complete)))
;;      (ac-config-default)
;;      (global-auto-complete-mode t)))

;;; [Outline Minor Mode]

;; Use C-c C-c as prefix (We use C-c as key prefix in Outline Major Mode)
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key "\C-c\C-c"
                                    outline-mode-prefix-map)))



;;;
;;; Functions for local tools
;;;

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(defun dos2unix()
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

;;(global-set-key "%" 'match-paren)
;;(defun match-paren (arg)
;;  "Go to the matching paren if on a paren; otherwise insert %."
;;  (interactive "p")
;;  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;	(t (self-insert-command (or arg 1)))))

;;;
;;; Platform-dependent settings
;;;

(defun arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)))


(if window-system
    (progn
      (blink-cursor-mode -1)
      (setq frame-title-format '(buffer-file-name "%f" ("b")))

      ;; Set frame size as pre-programmed from top of file
      (arrange-frame display-frame-width display-frame-height display-frame-left 0)

      ;; Maximize all frames by default.
      ;;(modify-all-frames-parameters '((fullscreen . maximized)))
      )
  (progn
    ))

(if (eq system-type 'gnu/linux)
    (progn

      ;; Shell in dired mode.
      (setq dired-guess-shell-alist-user
            `(
              (".rar" "unrar x")
              (".pdf" "evince")
              (".avi" "gxine")
              (".jpg" "eog")
              (".bmp" "eog")
              (".csv" "xdg-open")
              (".xlsx" "xdg-open")
              (".xls" "xdg-open")
              (".htm" "google-chrome")
              (".html" "google-chrome")
              ))
      ))

(if (eq system-type 'darwin)
    (progn

      ;; Use Option key as Meta key (consistent with Mac key bindings).
      (setq mac-option-modifier 'meta)

      ;; PC style handling of Home/End key.
      (global-set-key (kbd "<home>") 'move-beginning-of-line)
      (global-set-key (kbd "<end>") 'move-end-of-line)

      ;; "Ctrl+F4" in Mac is similar to "Alt-Tab" within the same workspace.
      (global-set-key (kbd "<C-f4>") 'other-frame)

      ;; Shell in dired mode.
      (setq dired-guess-shell-alist-user
            `(
              (".rar" "unrar x")
              (".jar" "jar xvf")
              (".pdf" "open -a Preview")
              (".jpg" "open -a Preview")
              (".png" "open -a Preview")
              (".md" "open -a Markoff")
              (".xcodeproj" "open -a Xcode")
              (".webloc" "open -a 'Google Chrome'")
              (".htm" "open -a 'Google Chrome'")
              (".html" "open -a 'Google Chrome'")
              ))

      ;; when launching Emacs from external program such as XCode
      (setq ns-pop-up-frames 'nil)

      ))

