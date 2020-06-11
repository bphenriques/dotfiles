;;; ~/Documents/repos/workspace/emacs/.config/doom/org-capture.el -*- lexical-binding: t; -*-

;;;
;;; Constants
;;;
(setq org-directory "~/org/")
(setq org-knowledge-base-repository "~/Documents/repos/knowledge-base")
(setq org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org"))

;;;
;;; Knowledge base
;;;
(defvar knowledge-base-capture-mode nil)
(defvar knowledge-base-capture-target-section nil)
(defvar knowledge-base-capture-entry-title nil)

(defun knowledge-base/org-capture/prepare ()
  "Required advice in order to share information between the function that returns the file and the function that returns the template"
  (let* ((sections (custom/list-directories org-knowledge-base-directory))
         (selected-section (completing-read "Section: " sections nil 'confirm)))

    (defun new-section ()
      (setq knowledge-base-capture-mode "create-section"))

    (defun new-entry ()
      (setq knowledge-base-capture-mode "create-entry")
      (let ((title (read-string "Title: ")))
        (when (not (equal title ""))
          (setf knowledge-base-capture-entry-title title))))

    (setq knowledge-base-capture-target-section selected-section)
    (cond ((member selected-section sections) (new-entry))
          ((y-or-n-p (format "Section '%s' does not exist! Create it?" selected-section)) (new-section))
          (t nil))))

(defun knowledge-base/org-capture/target-file ()
  (let ((mode knowledge-base-capture-mode)
        (section knowledge-base-capture-target-section)
        (title knowledge-base-capture-entry-title))

  (concat (file-name-as-directory org-knowledge-base-directory)
          (cond ((equal mode "create-section")
                 (format "%s/_index.org" (org-hugo-slug section)))
                ((equal mode "create-entry")
                 (format "%s/%s.org" (org-hugo-slug section) (org-hugo-slug title)))
                (t nil)))))

(defun knowledge-base/org-capture/template ()
  (knowledge-base/org-capture/prepare)
  (let ((mode knowledge-base-capture-mode))
    (cond ((equal mode "create-entry")
           (get-string-from-file (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org")))
          ((equal mode "create-section")
           (get-string-from-file (concat (file-name-as-directory org-knowledge-base-repository) "template-section.org")))
          (t nil))))


;;;
;;; Org Capture
;;;

(after! org-capture
  (setq org-capture-templates
        '(("f" "Fleeting note" entry (file "~/Documents/repos/knowledge-base/org/fleeting-notes.org")
           "* %?" :empty-lines 1)
          ("k" "New [k]nowledge base entry" plain
           (file knowledge-base/org-capture/target-file)
           (function knowledge-base/org-capture/template))
         )
    )
  )
