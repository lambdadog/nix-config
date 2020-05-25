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
  "Initialize deft in the background if it's required before
~deft-mode~ has been called."
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
  (let ((files (zettel--filter-string id nil t)))
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

(defconst zettel--tag-regexp-format-string "#\\+TAGS:.*? \\(?:#?\\)%s[ \n]"
  "Format string for converting a tag to a regexp that matches
  the tag. Can accept both non-prefixed and #-prefixed tags.")

(defun zettel--tag-regexp (tag)
  "Takes a tag (without #) and returns a regexp matching it in a
zettelkasten note."
  (format zettel--tag-regexp-format-string (regexp-quote tag)))

;; TODO: Deft assumes that files have been opened with it to hook
;; saves to update its cache. Either fix the root problem by adding a
;; hook to zettelkasten-mode or update cache inside zettel--validate.
(defun zettel--validate ()
  "Validate my zettelkasten to ensure that no public notes
forward-link to private notes."
  (let ((private-id-regexp (regexp-opt
			    (mapcar #'zettel--note-to-link-format
				    (zettel--filter-tag "private"))))
	(public-notes (zettel--filter-tag "public")))
    (let ((matches (zettel--filter-regexp private-id-regexp
					  public-notes)))
      (if (> (length matches) 0)
	  (progn
	    (message "Zettelkasten has links from public to private notes")
	    matches)
	matches))))

(defun zettel--note-to-link-format (note)
  "Format note ID to a link string that can be searched for."
  (let ((id (car (zettel--id-and-title note))))
    (concat "zettel:" id)))

(defun zettel--id-and-title (note)
  "Returns the ID and title of a note (must be passed by
filepath)."
  (let ((basename (file-name-base note)))
    (string-match "^\\(?1:[0-9\-]*\\) \\(?2:.*\\)$" basename)
    `(,(match-string-no-properties 1 basename) . ,(match-string-no-properties 2 basename))))

(defun zettel--filter (regexps &optional files filenames)
  "Filter files with a list of regexps. Files must be in the deft
cache for this function to work."
  (let ((deft-filter-regexp regexps)
	(deft-filter-only-filenames filenames)
	(deft-incremental-search nil))
    (deft-filter-files (if files files
			 deft-all-files))))

(defun zettel--filter-regexp (regexp &optional files filenames)
  "Filter files with a regexp. Files must be in the deft
cache for this function to work."
  (zettel--filter `(,regexp) files filenames))

(defun zettel--filter-string (string &optional files filenames)
  "Filter files with a string. Files must be in the deft
cache for this function to work."
  (zettel--filter `(,(regexp-quote string)) files filenames))

(defun zettel--filter-tag (tag &optional files)
  "Filter files by tag. Files must be in the deft cache for this
function to work."
  (zettel--filter `(,(zettel--tag-regexp tag)) files))

(defun zettel--filter-forwardlink (note &optional files)
  "Filter files by forward-link. Takes the path to the note you
wish to look for forward-links to. Files must be in the deft
cache for this function to work."
  (zettel--filter `(,(zettel--note-to-link-format note)) files))

(defconst zettel--all-tags-regexp "#\\+TAGS: \\(?:#?\\)\\(?1:.*\\)\n")
(defconst zettel--all-tags-separators " \\(?:#?\\)")

(defun zettel--get-tags ()
  "Gets the tags from the current buffer's zettelkasten note."
  (let ((tags '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward zettel--all-tags-regexp nil t)
        (let ((new-tags (split-string (match-string-no-properties 1)
				      zettel--all-tags-separators)))
	  (setq tags (append tags new-tags)))))
    tags))

(defconst zettel--forward-links-regexp "zettel:\\(?1:[0-9\-]+\\)")

(defun zettel--get-forwardlinks ()
  "Gets the forwardlinks from the current buffer's zettelkasten
note."
  (let ((forward-links '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward zettel--forward-links-regexp nil t)
	(push (match-string-no-properties 1) forward-links)))
    forward-links))

(defun zettel--get-backlinks ()
  "Gets the backlinks to the current buffer's zettelkasten note."
  (zettel--filter-forwardlink (buffer-file-name)))
