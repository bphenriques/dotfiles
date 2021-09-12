;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "6ec3d054ddadbca1f5effb961c1db583e377ca35")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "1795039ab93ef19611dbb3fca21c4211c4e655a9")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "9ad111d2102c24593f6ac012206bb4b2c9c6c4e1")
