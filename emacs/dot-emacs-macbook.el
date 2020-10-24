; -*- emacs-lisp -*-


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default))
 '(display-time-mode t)
 '(package-selected-packages
   '(switch-window web-mode graphviz-dot-mode solarized-theme elpy python-mode paredit flycheck yaml-mode rust-mode csharp-mode cmake-mode gist go-mode swift-mode ac-slime git-gutter markdown-mode cmake-project vterm julia-mode auto-complete))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Menlo")))))

;;; Variables required for Common Settings

(setq display-frame-width 88)
(setq display-frame-height 56)
(setq display-frame-left 715)

;;;
;;; Load Common Settings
;;;

(let ((local-init-file "~/.emacs.d/lisp/cwca/dotfiles/emacs/common.el"))
  (if (file-readable-p local-init-file)
      (load-file local-init-file)))


;; Recursivly put everything under lisp on the load path.
;; Put local or test versions of libraries there.

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;
;;; User Settings
;;;

(setq user-full-name "Chris Walsh")
(setq user-mail-address "chris@walshemail.ca")

;;;
;;; Local Settings
;;;

(setq default-directory "~/work/github/cwca/jig-lang/")
(if window-system
    (menu-bar-mode 1))
;;(load-theme 'leuven)
;;(load-theme 'wombat)


;; Set location for Sunrise/Sunset calculation

(setq calendar-location-name "Toronto, ON")
(setq calendar-latitude 43.67)
(setq calendar-longitude -79.37)

;;(setq calendar-location-name "16 Island Lake, QC")
;;(setq calendar-latitude   45.920833)
;;(setq calendar-longitude -74.466111)

;;
;; Modes under development
;;

(require 'jig-mode)

;;(if (and window-system (eq system-type 'darwin))
;;    (set-face-attribute 'default nil
;;                        :family "Menlo" :height 130 :weight 'normal))

