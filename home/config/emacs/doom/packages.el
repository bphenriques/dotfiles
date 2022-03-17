;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft") :pin "1b7054dcfc3547a7cafeb621552cec01d0540478")

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "cd1a55ff9820186c3896de78359458955583e2ee")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "3782e88d50f83c5b9fbb4b10df71df3f37e27156")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "1eb9abd4fccc7be767c5df1e158e2d17982f8193")

(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode") :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")
