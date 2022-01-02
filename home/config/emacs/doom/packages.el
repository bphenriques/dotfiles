;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft"))

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "1970cf900dcf7f1880a79b15ffc1f70c3891dbb0")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "ee9a8d423e24923ec60c3116a00c8a0e93095d86")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "9ed0c5705a302a91fab2b8bcc777a12dcf9b3682")
