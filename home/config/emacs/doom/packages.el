;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "1b8f2627cd63ac21b84c5abe3d5b607bc778670a")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "9065f6a999b98d4b495e3d8fa1fa4424eddd25a8")

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server") :pin "a207ecd36e29dad55eb66431f041e39144130ee5")
