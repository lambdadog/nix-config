;; -*- lexical-binding: t -*-

;; The trailing slash is important here for matching with
;; default-directory to enable zettelkasten-mode
(setq zettel-dir (expand-file-name "~/notes/zettel/"))
(setq zettel-id-format "%Y-%m-%d-%H%M%S")

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft-mode)
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

;; Org links

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
  (let ((deft-filter-only-filenames t))
    (deft-filter id t))
  (unless (eq (length deft-current-files) 1)
    (user-error "ID Error. Either no or multiple zettelkasten notes found with ID %s" id))
  (deft-open-file (car deft-current-files)))

(defun org-zettel-store-link ()
  "Store a link to a zettel note."
  (when zettelkasten-mode
    (let* ((basename (file-name-base (buffer-file-name)))
	   (zettel-id (progn
			(string-match "^[0-9\-]*" basename)
			(match-string 0 basename)))
	   (zettel-title (progn
			   (string-match "\\(?:^[0-9\-]*\\) \\(?1:.*\\)$" basename)
			   (match-string 1 basename))))
      (org-store-link-props
       :type "zettel"
       :link (concat "zettel:" zettel-id)
       :description zettel-title))))

;; Helper functions

(defun zettel-new-note (title)
  "Create a new zettelkasten note."
  (interactive "sTitle: ")
  (let* ((zettel-id (format-time-string zettel-id-format))
	 (filename (concat zettel-id " " title)))
    (deft-new-file-named filename)
    filename))
  
