;;; -*- lexical-binding: t; -*-

(require 'ob-scala-cli)

(after! org-mode
  :config
    (add-to-list 'org-babel-load-languages '(scala-cli . t) '(scala . t))
    (setq ob-scala-cli-default-params '(:scala-version "3.3.1" :jvm 21)))
