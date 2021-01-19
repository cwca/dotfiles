; -*- emacs-lisp -*-

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Menlo")))))


;;; Variables required for Common Settings

;; font size 130
;;(setq display-frame-width 224)
;;(setq display-frame-height 92)
;;(setq display-frame-left 740)

;; font size 150
(setq display-frame-width 199)
(setq display-frame-height 77)
(setq display-frame-left 740)

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

;;(if window-system
;;    (load-theme 'adwaita)
;;  (load-theme 'wombat))


;; Set location for Sunrise/Sunset calculation

(setq calendar-location-name "Toronto, ON")
(setq calendar-latitude 43.67)
(setq calendar-longitude -79.37)

;;(setq calendar-location-name "16 Island Lake, QC")
;;(setq calendar-latitude   45.920833)
;;(setq calendar-longitude -74.466111)

;; OCaml

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var"
   "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

;;
;; Modes under development
;;

(require 'jig-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default))
 '(package-selected-packages '(switch-window cmake-project vterm auto-complete)))
