;; -*- lexical-binding: t -*-

;;; Dashboard
(use-package dashboard
  :commands (dashboard-insert-startupify-lists)
  :custom
  (dashboard-set-init-info nil)
  (dashboard-set-footer nil))

;; 4 instead of 2 because we launch emacs with --load file already
(when (< (length command-line-args) 4)
  (require 'dashboard)
  ;; suppress "Cleaning up the recentf list...done" message
  (let ((inhibit-message t))
    (dashboard-insert-startupify-lists))
  (add-hook 'emacs-startup-hook
	    #'(lambda ()
		(switch-to-buffer "*dashboard*")
		(goto-char (point-min))
		(redisplay))))
