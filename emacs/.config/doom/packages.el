;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "95723cd")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "1b598a461849318fca2df3917c254d8ee483a87e")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server") :pin "fde2636d794f020ed5810fa38fe57a264932f661")
