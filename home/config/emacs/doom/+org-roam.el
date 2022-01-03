;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq
 org-knowledge-base-repository "~/workspace/knowledge-base/"
 org-knowledge-base-directory (concat (file-name-as-directory org-knowledge-base-repository) "org")
 org-knowledge-base-template (concat (file-name-as-directory org-knowledge-base-repository) "template.org")
 dailies-directory "~/workspace/dailies/")

(after! org-mode
 :config
 (setq
   org-id-link-to-org-use-id t                                                                ; Use ids when linking files.
   org-id-extra-files (directory-files-recursively org-knowledge-base-directory "\.org$")))   ; Follow ids in org-roam.

(after! ox-hugo
  :config
  (setq org-hugo-base-dir org-knowledge-base-repository))

;; ox-hugo file based exports rely on "#+HUGO_TAGS" and not on "#+FILETAGS" org-roam does. This syncs both values.
;; If filetags is set, overwrite hugo_tags with its values.
;; If filetags is not set but hugo_tags is, set filetags with the value of hugo_tags.
(defun sync-hugo-tags ()
  (interactive)
  (let ((filetags (car (cdr (car (org-collect-keywords '("filetags"))))))
        (hugotags (car (cdr (car (org-collect-keywords '("HUGO_TAGS")))))))
    (if filetags
        (org-roam-set-keyword "hugo_tags" filetags)
        (when hugotags (org-roam-set-keyword "filetags" hugotags)))))

(add-hook 'before-save-hook 'sync-hugo-tags) ;; Using before so that it works during the capture process.

(after! org-roam
  :init
  (map! :leader
      (:prefix-map ("k" . "knowledge-base")
       :desc "Roam Toggle Side Pane"    "l"    #'org-roam-buffer-toggle
       :desc "Roam Insert"              "i"    #'org-roam-node-insert
       :desc "Roam Node Find"           "f"    #'org-roam-node-find
       :desc "Global Find"              "F"    #'notdeft
       :desc "Roam UI"                  "g"    #'org-roam-ui-open
       :desc "Roam Capture"             "c"    #'org-roam-capture
       (:prefix-map ("d" . "Daily")
        :desc "Capture Today"           "T"    #'org-roam-dailies-capture-today
        :desc "Goto Today"              "t"    #'org-roam-dailies-goto-today
        :desc "Capture Yesterday"       "Y"    #'org-roam-dailies-capture-yesterday
        :desc "Goto Yesterday"          "y"    #'org-roam-dailies-goto-yesterday
        :desc "Capture Date"            "D"    #'org-roam-dailies-goto-date
        :desc "Goto Date"               "d"    #'org-roam-dailies-capture-date
        :desc "List"                    "l"    #'org-roam-dailies-find-directory)))

  (setq org-roam-directory org-knowledge-base-directory)
  (org-roam-db-autosync-mode) ; Ensure org-roam is available at startup.
  :config
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain
           "%?"
           :if-new (file+head "${slug}.org" "#+title: ${title}\n#+filetags: ")
           :empty-lines 1
           :kill-buffer t
           :unnarrowed t)
          )))

(after! org-roam-dailies
  :config
  (setq org-roam-dailies-directory dailies-directory)
  (setq org-roam-dailies-capture-templates
        '(
          ("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: ")
           :empty-lines 1
           :kill-buffer t
           :unnarrowed t)
           )))
