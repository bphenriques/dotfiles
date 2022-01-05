;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft"))

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "de5dfdfdcdc3ec50cb27d1f6cf7314a5c9aa1385")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "679ef6ef001fd1a69b691108178721aa913e7f0f")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "9ed0c5705a302a91fab2b8bcc777a12dcf9b3682")

(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode") :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")
