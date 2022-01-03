;;; -*- lexical-binding: t; -*-

(custom-set-variables
 '(safe-local-variable-values
   '(
     ;; Dailies
     (gac-automatically-push-p . t)
     (gac-automatically-add-new-files-p . t)
     (gac-commit-additional-flag . "--no-gpg-sign")
     ;; Knowledge-base
     (org-roam-file-exclude-regexp . "_index.org")
     (eval setq-local org-roam-directory
                (concat
                 (file-name-as-directory
                  (projectile-project-root))
                 "org")))))
