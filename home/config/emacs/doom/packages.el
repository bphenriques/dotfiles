;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "a86ea7ed782f3e04f67329f95109749ad231c60a")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "b0fd12647b94ba6e3cf82a2a5b1ee7655ac07760")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server") :pin "2093ea5a1a1f2d128dd377778472a481913717b4")
