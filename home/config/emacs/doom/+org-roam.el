;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq
  org-knowledge-base-directory "~/workspace/knowledge-base/org"
  dailies-directory "~/workspace/dailies/")

(after! org-mode
  :config
  (setq
    org-id-link-to-org-use-id t                                                                ; Use ids when linking files.
    org-id-extra-files (directory-files-recursively org-knowledge-base-directory "\.org$")))   ; Follow ids in org-roam.

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

  (setq org-roam-directory org-knowledge-base-directory)    ; Ensure org-roam is pointing to my knowledge-base by default.
  (org-roam-db-autosync-mode)                               ; Ensure org-roam is available at startup.
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
