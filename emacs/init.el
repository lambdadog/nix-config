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

;; Helper functions
(setq config-home (if load-file-name
		      (file-name-directory load-file-name)
		    default-directory))

(defun load-config (path)
  (let ((file (expand-file-name (concat config-home path))))
    (load file nil t t)))

;; Load up all of our autoloads, since nix doesn't do that for us
(load-config "autoload.el")

(eval-when-compile
  (setq use-package-always-defer t)
  (require 'use-package))

;;; Config
(load-config "zettel.el")
