;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "95723cd")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "7602b8c48d8c5077a93b533066db1127ebfce597")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server") :pin "fde2636d794f020ed5810fa38fe57a264932f661")
