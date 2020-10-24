;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "95723cd")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "a7cf48e")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server") :pin "1dc94e102d60e53bb9929b1cdc55d4d8c2b0d64f")
