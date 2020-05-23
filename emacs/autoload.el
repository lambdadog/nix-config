;; -*- lexical-binding: t -*-

(dolist (dir load-path)
  (dolist (autoload (file-expand-wildcards
		     (expand-file-name "*-autoloads.el" dir)
		     t))
    (load autoload nil t t)))
