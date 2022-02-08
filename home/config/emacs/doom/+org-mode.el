;;; ../../Documents/repos/dotfiles/emacs/.config/doom/+org-mode.el -*- lexical-binding: t; -*-

(after! org
  (setq
   org-directory "~/org/"
   org-ellipsis "▼"                        ; Icon for collapsed items
   org-hide-emphasis-markers t             ; Hide markers
   org-startup-with-inline-images nil      ; do not inline images by default
))


(after! org
  (setq-default org-download-image-dir "./images")
  (setq-default org-download-heading-lvl nil)
  (setq org-download-method 'directory))
