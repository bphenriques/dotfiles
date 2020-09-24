;;; ../../Documents/repos/dotfiles/emacs/.config/doom/+keybindings.el -*- lexical-binding: t; -*-

(after! org-roam
  (map! :leader
         (:prefix-map ("k" . "knowledge-base")
          :desc "Roam Insert"    "i"    #'org-roam-insert
          :desc "Roam Find"      "f"    #'org-roam-find-file
          :desc "Deft file"      "F"    #'deft
          :desc "Roam Server"    "g"    #'org-roam-server-open
          :desc "Roam Capture"   "c"    #'org-roam-capture)))
