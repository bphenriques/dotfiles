;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft"))

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "f0d06bdb8a2f6256f0e71e085e2761ea8e0c3a5c")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "d20480bb8d86764d0ce37906fcda0484b46ee7fa")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "309fe3c58c7081de4e2c9c64f7b40ea291926048")

(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode") :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")
