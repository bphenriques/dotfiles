;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "95723cd")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "369753c")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server") :pin "8d1d143")
