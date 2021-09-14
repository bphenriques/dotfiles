;;; ~/Documents/repos/workspace/emacs/.config/doom/+org-roam.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq
 org-knowledge-base-repository "~/workspace/knowledge-base"
 org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org")
 org-knowledge-base-entry-template (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org")

 ; Org-roam set globally to take notes anywhere.
 ; - Due to https://github.com/org-roam/org-roam-server/issues/8 org-roam-server does not respect .dir-locals.el
 org-roam-directory org-knowledge-base-directory
 org-roam-file-exclude-regexp "_index.org"
 org-roam-tag-sources '(prop)

 ; Org-roam-server
 org-roam-server-port 8090)

(after! org-roam
 (set-face-attribute 'org-roam-tag nil :foreground "tan1")
 (setq org-roam-capture-templates
        `(
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head ,(get-string-from-file org-knowledge-base-entry-template)
           :unnarrowed t)
          )))

;;;
;;; Org-Roam Server
;;;
;;; Requires Org-Server Protocol
;;;

(defun org-roam-server-open ()
  "Ensure the server is active, then open the roam graph."
  (interactive)
  (when (eq org-roam-server-mode nil)
    (org-roam-server-mode 1))
  (call-process "open" nil nil nil (format "http://%s:%d" org-roam-server-host org-roam-server-port)))
