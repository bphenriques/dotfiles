;; -*- no-byte-compile: t; -*-

(package! notdeft
  :recipe (:host github :repo "hasu/notdeft") :pin "1b7054dcfc3547a7cafeb621552cec01d0540478")

(package! ox-hugo
  :recipe (:host github :repo "kaushalmodi/ox-hugo") :pin "cb1b6cfd7b080e889352150416c1725f11ba937a")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam") :pin "5c06471c3a11348342719fd9011486455adeb701")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui") :pin "6bf6a5eecc1fa7ddbb1fcda85e08fe9c393f9298")

(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode") :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")

(package! scala-cli-repl
  :recipe (:host github :repo "ag91/scala-cli-repl" :files ("*.el")) :pin "e96dcdfaa5f8d7215103e55e59a491dacd671984")

(package! synosaurus
  :recipe (:host github :repo "hpdeifel/synosaurus" :files ("*.el")) :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81")
