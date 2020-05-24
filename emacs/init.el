;; -*- lexical-binding: t -*-

;;; Header

;; Load our theme as early as possible
;;
;; We don't use load-theme here because that would require loading the
;; entire 'doom-themes package, which is unnecessary. May introduce
;; some jank if you change the theme while emacs is running though.
(require 'doom-sourcerer-theme)

;; These also should be quick to look good
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Neuter package.el
(setq package-enable-at-startup nil)

;; I'm not sure if these are an issue since we don't use ~/.emacs.d,
;; but it can't hurt to override them just in case they intend to
;; leave a mess.
(defun package--ensure-init-file/:override ())
(advice-add #'package--ensure-init-file :override
	    #'package--ensure-init-file/:override)
(defun package--save-selected-packages/:override (&optional _))
(advice-add #'package--save-selected-packages :override
	    #'package--save-selected-packages/:override)

;; Helper functions
(setq config-home (if load-file-name
		      (file-name-directory load-file-name)
		    default-directory))

(defun load-config (path)
  (let ((file (expand-file-name (concat config-home path))))
    (load file nil t t)))

;; Load up all of our autoloads, since nix doesn't do that for us
;;
;; We don't need to worry about the other loadable file extensions
;; because nix always generates autoloads as a .el file, so we can
;; just pretend they don't exist.
(dolist (dir load-path)
  (dolist (autoload (file-expand-wildcards
		     (expand-file-name "*-autoloads.el" dir)
		     t))
    (load autoload nil t t)))

(eval-when-compile
  (setq use-package-always-defer t)
  (require 'use-package))

(require 'ryo-modal)

;;; Config
(load-config "ui.el")
(load-config "zettel.el")
