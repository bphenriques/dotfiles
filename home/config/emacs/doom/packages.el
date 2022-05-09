;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft") :pin "1b7054dcfc3547a7cafeb621552cec01d0540478")

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "3c699a1ed3f666735974fb90610d60b41e86a3b2")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "007e76725cdee4b16c003aec096e2dd7ab399b56")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "9474a254390b1e42488a1801fed5826b32a8030b")

(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode") :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")
