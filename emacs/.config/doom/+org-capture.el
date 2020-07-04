;;; ~/Documents/repos/workspace/emacs/.config/doom/org-capture.el -*- lexical-binding: t; -*-

;;;
;;; Constants
;;;
(setq
 org-directory "~/org/"
 org-knowledge-base-repository "~/Documents/repos/knowledge-base"
 org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org")
 org-knowledge-base-section-template (concat (file-name-as-directory org-knowledge-base-repository) "template-section.org")
 org-knowledge-base-entry-template (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org"))

;;;
;;; Knowledge base
;;;

(defun knowledge-base/new-section/template ()
  (let* ((sections (custom/list-directories org-knowledge-base-directory))
         (section (completing-read "Section: " sections)))

    (if (member section sections)
        (error (concat "Section '%s' already exists! Skipping." section))
      (progn
        (org-capture-put :knowledge-base-section section)
        (get-string-from-file org-knowledge-base-section-template)))))

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
        (get-string-from-file org-knowledge-base-entry-template)))))

(defun knowledge-base/new-fleeting-entry/file ()
  (let ((title (org-capture-get :knowledge-base-title)))
    (concat (file-name-as-directory org-knowledge-base-directory)
            (format "%s.org" (org-hugo-slug title)))))

(defun knowledge-base/entry/template ()
  (let* ((sections (custom/list-directories org-knowledge-base-directory))
         (section (completing-read "Section: " sections nil t)) ; guarantee one option is selected
         (section-directory (concat (file-name-as-directory org-knowledge-base-directory) section))
         (entries (directory-files section-directory nil "\\.org$"))
         (entry (completing-read "Entry: " entries nil 'confirm)) ; can create new entry
         (entry-path (concat (file-name-as-directory section-directory) entry)))

    (cond ((file-exists-p entry-path)
           (org-capture-put :knowledge-base-entry-path entry-path)
           "%?")
          (t
           (org-capture-put :knowledge-base-title entry)
           (org-capture-put :knowledge-base-entry-path (concat
                                                        (file-name-as-directory section-directory)
                                                        (format "%s.org" (org-hugo-slug entry))))
           (get-string-from-file org-knowledge-base-entry-template)))))

(defun knowledge-base/entry/file ()
  (org-capture-get :knowledge-base-entry-path))

;;;
;;; Org Capture
;;;

(after! org-capture
  (setq org-capture-templates
        '(
          ("k" "Knowledge Base")
          ("kf" "[f]leeting entry" plain (file knowledge-base/new-fleeting-entry/file) (function knowledge-base/new-fleeting-entry/template))
          ("ks" "[s]ection" plain (file knowledge-base/new-section/file) (function knowledge-base/new-section/template) :immediate-finish t)
          ("ke" "[e]ntry" plain (file knowledge-base/entry/file) (function knowledge-base/entry/template) :kill-buffer t :unnarrowed t)
          )
        ))
