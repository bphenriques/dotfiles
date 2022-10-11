;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft") :pin "1b7054dcfc3547a7cafeb621552cec01d0540478")

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "e532fce4a4fbf9a7981bed07d1b9ec30a4d0a305")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "d95d25615e69e7cc847641800c1886366336c97e")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "6bf6a5eecc1fa7ddbb1fcda85e08fe9c393f9298")

(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode") :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")
