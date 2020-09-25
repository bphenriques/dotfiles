;;; ~/Documents/repos/workspace/emacs/.config/doom/+org-roam.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq
 org-knowledge-base-repository "~/Documents/repos/knowledge-base"
 org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org")
 org-knowledge-base-section-template (concat (file-name-as-directory org-knowledge-base-repository) "template-section.org")
 org-knowledge-base-entry-template (concat (file-name-as-directory org-knowledge-base-repository) "template-entry.org")
 org-knowledge-base-web-entry-template (concat (file-name-as-directory org-knowledge-base-repository) "template-web-entry.org")
 org-knowledge-base-stack-web-entries-template (concat (file-name-as-directory org-knowledge-base-repository) "template-stack-web-entries.org")
 org-knowledge-base-quick-note-section "uncategorized"

 ; Org-roam set globally to take notes anywhere.
 ; - Due to https://github.com/org-roam/org-roam-server/issues/8 org-roam-server does not respect .dir-locals.el
 org-roam-directory org-knowledge-base-directory
 org-roam-file-exclude-regexp "_index.org"
 org-roam-tag-sources '(prop)

 ; Org-roam-server
 org-roam-server-port 8090)

;;;
;;; Org-Roam Capture
;;;

(defun knowledge-base/prompt-section ()
  (let* ((sections (cons  org-knowledge-base-quick-note-section (remove  org-knowledge-base-quick-note-section (custom/list-directories org-knowledge-base-directory))))
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

(after! org-roam
 (setq org-roam-capture-templates
        `(
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%(knowledge-base/prompt-section)/${slug}"
           :head ,(get-string-from-file org-knowledge-base-entry-template)
           :unnarrowed t)
          )))

;;;
;;; Org-Roam Ref Capture
;;;

(after! org-roam
 (setq org-roam-capture-ref-templates
        `(
          ;; javascript:location.href ='org-protocol://roam-ref?template=n&ref='+ encodeURIComponent(location.href)+ '&title='+ encodeURIComponent(document.title)
          ("n" "Note Reference" plain (function org-roam--capture-get-point)
           "%?"
           :file-name ,(format "%s/web-note__${slug}" org-knowledge-base-quick-note-section)
           :head ,(get-string-from-file org-knowledge-base-web-entry-template)
           :unnarrowed t)
          ;; javascript:location.href ='org-protocol://roam-ref?template=e&ref='+ encodeURIComponent(location.href)+ '&title='+ encodeURIComponent(document.title)
           ("e" "Enqueue Reference" plain (function org-roam--capture-get-point)
           "\n* ${title}\n\nSource: ${ref}\n\n%?"
           :file-name ,(format "%s/web-stack-enties" org-knowledge-base-quick-note-section)
           :head ,(get-string-from-file org-knowledge-base-stack-web-entries-template)
           :unnarrowed t :empty-lines-before 1)
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
