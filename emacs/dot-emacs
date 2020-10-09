; -*- emacs-lisp -*-

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
    (load-theme 'tango)
  (load-theme 'wombat))


;; Set location for Sunrise/Sunset calculation

;;(setq calendar-location-name "Toronto, ON")
;;(setq calendar-latitude 43.67)
;;(setq calendar-longitude -79.37)

(setq calendar-location-name "16 Island Lake, QC")
(setq calendar-latitude   45.920833)
(setq calendar-longitude -74.466111)



