;; -*- lexical-binding: t -*-

(dolist (load-suffix (get-load-suffixes))
  (dolist (dir load-path)
    (let* ((glob      (concat "*-autoloads" load-suffix))
	   (fullglob  (expand-file-name glob dir))
	   (autoloads (file-expand-wildcards fullglob t)))
      (dolist (autoload autoloads)
	(load autoload nil t t)))))
