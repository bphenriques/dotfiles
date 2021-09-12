;;; -*- lexical-binding: t -*-

(require 'cl-lib)

;;; https://www.orgroam.com/manual.html

(setq
 org-knowledge-base-repository "~/workspace/knowledge-base"
 org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org")
 org-knowledge-base-entry-template (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org")

 ;; HUGO Setup
 org-hugo-base-dir org-knowledge-base-directory

 ;; org-roam
 org-roam-directory org-knowledge-base-directory)

(defun org-hugo--tag-processing-fn-roam-tags(tag-list info)
  "Process org roam tags for org hugo"
  (if (org-roam--org-roam-file-p)
      (org-roam--extract-tags)
      tag-list))

(after! ox-hugo
 (add-to-list 'org-hugo-tag-processing-functions 'org-hugo--tag-processing-fn-roam-tags))

(after! org-roam
 (org-roam-db-autosync-mode))

;;;  org-file-tags #+filetags
;;;  org-roam-file-exclude-regexp "_index.org")


;;;
;;; Org-Roam Capture
;;;

