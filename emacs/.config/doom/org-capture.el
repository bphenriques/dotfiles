;;; ~/Documents/repos/workspace/emacs/.config/doom/org-capture.el -*- lexical-binding: t; -*-

;;;
;;; Constants
;;;
(setq org-directory "~/org/")
(setq org-knowledge-base-repository "~/Documents/repos/knowledge-base")
(setq org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org"))
(setq org-knowledge-exported-content-directory (concat (file-name-as-directory org-knowledge-base-directory) "content"))

;;;
;;; Knowledge base
;;;

(defun knowledge-base/new-entry/file ()
  (let ((title (org-capture-get :knowledge-base-title))
        (section (org-capture-get :knowledge-base-section)))

    (concat (file-name-as-directory org-knowledge-base-directory)
            (format "%s/%s.org" (org-hugo-slug section) (org-hugo-slug title)))))

(defun knowledge-base/new-entry/template ()
  (let* ((sections (custom/list-directories org-knowledge-base-directory))
         (section (completing-read "Section: " sections nil t)) ; guarantee one option is selected
         (title (read-string "Title: ")))

    (if (equal title "")
        (error "Empty title!")
      (progn
        (org-capture-put :knowledge-base-section section :knowledge-base-title title)
        (get-string-from-file (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org"))))))

(defun knowledge-base/new-section/template ()
  (let* ((sections (custom/list-directories org-knowledge-base-directory))
         (section (completing-read "Section: " sections)))

    (if (member section sections)
        (error (concat "Section '%s' already exists! Skipping." section))
      (progn
        (org-capture-put :knowledge-base-section section)
        (get-string-from-file (concat (file-name-as-directory org-knowledge-base-repository) "template-section.org"))))))

(defun knowledge-base/new-section/file ()
  (let ((section (org-capture-get :knowledge-base-section)))
    (concat (file-name-as-directory org-knowledge-base-directory)
            (format "%s/_index.org" (org-hugo-slug section)))))

(defun knowledge-base/new-fleeting-entry/template ()
  (let* ((title (read-string "Title: ")))
    (if (equal title "")
        (error "Empty title!")
      (progn
        (org-capture-put :knowledge-base-title title)
        (get-string-from-file (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org"))))))

(defun knowledge-base/new-fleeting-entry/file ()
  (let ((title (org-capture-get :knowledge-base-title)))
    (concat (file-name-as-directory org-knowledge-base-directory)
            (format "%s.org" (org-hugo-slug title)))))

;;;
;;; Org Capture
;;;

(after! org-capture
  (setq org-capture-templates
        '(
          ("k" "Knowledge Base")
          ("ks" "New [s]ection" plain (file knowledge-base/new-section/file) (function knowledge-base/new-section/template) :immediate-finish t)
          ("ke" "New [e]ntry" plain (file knowledge-base/new-entry/file) (function knowledge-base/new-entry/template))
          ("kf" "New [f]leeting entry" plain (file knowledge-base/new-fleeting-entry/file) (function knowledge-base/new-fleeting-entry/template))
          )
        ))
