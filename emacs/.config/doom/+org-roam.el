;;; ~/Documents/repos/workspace/emacs/.config/doom/+org-roam.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq
 org-roam-tag-sources '(prop last-directory)
 org-roam-graph-viewer "open")

(setq
 org-knowledge-base-repository "~/Documents/repos/knowledge-base"
 org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org")
 org-knowledge-base-section-template (concat (file-name-as-directory org-knowledge-base-repository) "template-section.org")
 org-knowledge-base-entry-template (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org"))

;;;
;;; Knowledge base
;;;

(defun knowledge-base/prompt-section ()
   (let* ((sections (custom/list-directories org-knowledge-base-directory))
          (section (completing-read "Section: " sections)))
     (org-capture-put :knowledge-base-section section)
     (org-hugo-slug section)))

(defun knowledge-base/setup-section-if-needed ()
  (let* ((section (org-capture-get :knowledge-base-section))
         (section-directory (concat (file-name-as-directory (concat (file-name-as-directory org-knowledge-base-directory) (org-hugo-slug section)))))
         (section-index-file (concat (file-name-as-directory section-directory) "_index.org")))

    (when (not (file-exists-p section-index-file))
      (append-to-file (replace-regexp-in-string"${section-slug}"
                                                (org-hugo-slug section)
                                                (replace-regexp-in-string "${section-title}"
                                                                          section
                                                                          (get-string-from-file org-knowledge-base-section-template)))
                      nil
                      section-index-file))))

(add-hook 'org-capture-after-finalize-hook 'knowledge-base/setup-section-if-needed)

;;;
;;; Org Capture
;;;
(after! org-roam
  (setq org-roam-capture-templates
        `(
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%(knowledge-base/prompt-section)/${slug}"
           :head ,(get-string-from-file org-knowledge-base-entry-template)
           :unnarrowed t)
        )))
