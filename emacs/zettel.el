;; -*- lexical-binding: t -*-

(use-package deft
  :commands (deft
	     deft-refresh)
  :custom
  (deft-extensions '("org"))
  (deft-directory "~/zettel")
  (deft-use-filename-as-title t))

(use-package zetteldeft
  :commands (zetteldeft-deft-new-search
	     zetteldeft-search-at-point
	     zetteldeft-search-current-id
	     zetteldeft-follow-link
	     zetteldeft-avy-file-search-ace-window
	     zetteldeft-avy-link-search
	     zetteldeft-avy-tag-search
	     zetteldeft-tag-buffer
	     zetteldeft-find-file-id-insert
	     zetteldeft-find-file-full-title-insert
	     zetteldeft-find-file
	     zetteldeft-new-file
	     zetteldeft-new-file-and-link
	     zetteldeft-file-rename
	     zetteldeft-count-words))

(defhydra lambdadog/zettelkasten-dumb (:exit t)
  "Zettelkasten"
  ("d" deft "deft")
  ("D" zetteldeft-deft-new-search "zetteldeft-deft-new-search")
  ("R" deft-refresh "deft-refresh")
  ("s" zetteldeft-search-at-point "zetteldeft-search-at-point")
  ("c" zetteldeft-search-current-id "zetteldeft-search-current-id")
  ("f" zetteldeft-follow-link "zetteldeft-follow-link")
  ("F" zetteldeft-avy-file-search-ace-window "zetteldeft-avy-file-search-ace-window")
  ("l" zetteldeft-avy-link-search "zetteldeft-avy-link-search")
  ("t" zetteldeft-avy-tag-search "zetteldeft-avy-tag-search")
  ("T" zetteldeft-tag-buffer "zetteldeft-tag-buffer")
  ("i" zetteldeft-find-file-id-insert "zetteldeft-find-file-id-insert")
  ("I" zetteldeft-find-file-full-title-insert "zetteldeft-find-file-full-title-insert")
  ("o" zetteldeft-find-file "zetteldeft-find-file")
  ("n" zetteldeft-new-file "zetteldeft-new-file")
  ("N" zetteldeft-new-file-and-link "zetteldeft-new-file-and-link")
  ("r" zetteldeft-file-rename "zetteldeft-file-rename")
  ("x" zetteldeft-count-words "zetteldeft-count-words"))

(global-set-key (kbd "C-c d") 'lambdadog/zettelkasten-dumb/body)
