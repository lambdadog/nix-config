;; -*- lexical-binding: t -*-

;; The trailing slash is important here for matching with
;; default-directory to enable zettelkasten-mode
(setq zettel-dir (expand-file-name "~/notes/zettel/"))
(setq zettel-id-format "%Y-%m-%d-%H%M%S")

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft-mode
	     deft-new-file-named)
  :custom
  (deft-extensions '("org"))
  (deft-directory zettel-dir)
  (deft-use-filename-as-title t))

;; We need a minor mode so that we can create ryo-modal bindings for
;; working with my zettelkasten.
(define-minor-mode zettelkasten-mode
  "A mode for editing org files in my zettelkasten"
  :lighter " Zettel")

(add-hook 'org-mode-hook #'org-mode-hook/zettelkasten-mode)
(defun org-mode-hook/zettelkasten-mode ()
  (when (string= default-directory zettel-dir)
    (zettelkasten-mode 1)))

;; Org zettel:* links

;; TODO: Implement :export
;; TODO: Implement :face
;; TODO: Evaluate implementing :complete
(org-link-set-parameters "zettel"
                         :follow #'org-zettel-follow-link
                         :store #'org-zettel-store-link)

(defun zettel--deft-init-in-background ()
  (message "Initializing deft...")
  (with-current-buffer (get-buffer-create "*Deft*")
    (if (not (eq major-mode 'deft-mode))
	(let ((inhibit-message t))
	  (deft-mode))))
  (message "Done"))

(defun org-zettel-follow-link (id)
  "Follow a zettel: link in org-mode."
  (unless (get-buffer "*Deft*")
    (zettel--deft-init-in-background))
  (let ((files (let ((deft-filter-regexp `(,id))
		     (deft-filter-only-filenames t))
		 (deft-filter-files deft-all-files))))
    (unless (eq (length files) 1)
      (user-error "ID Error. Either no or multiple zettelkasten notes found with ID %s" id))
    (deft-open-file (car files))))

(defun org-zettel-store-link ()
  "Store a link to a zettel note."
  (when zettelkasten-mode
    (let ((zettel-info (zettel--id-and-title (buffer-file-name))))
      (org-store-link-props
       :type "zettel"
       :link (concat "zettel:" (car zettel-info))
       :description (cdr zettel-info)))))

;; User-facing functions

(defun zettel--new-note (title)
  "Create a new zettelkasten note."
  (let* ((zettel-id (format-time-string zettel-id-format))
	 (filename (concat zettel-id " " title)))
    (deft-new-file-named filename)
    (insert (concat "#+TITLE: " title "\n"
		    "#+TAGS: "))
    filename))

(defun zettel-new-public-note (title)
  "Create a new private zettelkasten note."
  (interactive "sTitle: ")
  (let ((filename (zettel-new-note title)))
    (insert "#public ")
    filename))

(defun zettel-new-private-note (title)
  "Create a new private zettelkasten note."
  (interactive "sTitle: ")
  (let ((filename (zettel-new-note title)))
    (insert "#private ")
    filename))

(defun zettel-post-to-blog (&optional note)
  "Post this zettelkasten note as a blog post."
  (interactive)
  (let* ((note (if zettelkasten-mode
		  (buffer-file-name)
		 note))
	 (zettel-id (car (zettel--id-and-title note))))
    ;; Write to a blog.txt file with date, I guess?
    (error "Unimplemented")))
    
(defun zettel-publish-blog ()
  "Publish my blog."
  (interactive)
  (error "Unimplemented"))

;; Internal

(defconst zettel--private-regexp "#\\+TAGS:.+?#private.*\n")
(defconst zettel--public-regexp "#\\+TAGS:.+?#public.*\n")

;; TODO: Deft assumes that files have been opened with it to hook
;; saves to update its cache. Either fix the root problem by adding a
;; hook to zettelkasten-mode or update cache inside zettel--validate.
(defun zettel--validate ()
  "Validate my zettelkasten to ensure that no public notes
forward-link to private notes."
  (let ((private-id-regexp (regexp-opt
			    (let ((files (let ((deft-filter-regexp `(,zettel--private-regexp))
					       (deft-incremental-search nil))
					   (deft-filter-files deft-all-files))))
			      (mapcar #'zettel--validate--note-to-link-format files))))
	(public-notes (let ((deft-filter-regexp `(,zettel--public-regexp))
			    (deft-incremental-search nil))
			(deft-filter-files deft-all-files))))
    (let ((matches
	   (let ((deft-filter-regexp `(,private-id-regexp))
		 (deft-incremental-search nil))
	     (deft-filter-files public-notes))))
      (if (> (length matches) 0)
	  (progn
	    (message "Zettelkasten has links from public to private notes")
	    matches)
	matches))))

(defun zettel--validate--note-to-link-format (note)
  (let ((id (car (zettel--id-and-title note))))
    (concat "[[zettel:" id "]")))

(defun zettel--id-and-title (note)
  (let ((basename (file-name-base note)))
    (string-match "^\\(?1:[0-9\-]*\\) \\(?2:.*\\)$" basename)
    `(,(match-string 1 basename) . ,(match-string 2 basename))))
