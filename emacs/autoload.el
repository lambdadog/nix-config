;; -*- lexical-binding: t -*-

(defun lambdadog/load-autoload (path)
  (load path nil t t))

(dolist (dir load-path)
  (let ((autoload-globs (mapcar (apply-partially #'concat "*-autoloads")
				(get-load-suffixes))))
    (dolist (glob autoload-globs)
      (let* ((fullglob       (expand-file-name glob dir))
	     (autoloads      (file-expand-wildcards fullglob t)))
	(mapcar #'lambdadog/load-autoload autoloads)))))
