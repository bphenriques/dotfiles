;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; No need to run 'doom sync' after modifying this file.

;; Fix MacOS
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(setq user-full-name "Bruno Henriques"
      user-mail-address "john@doe.com")

;; Theming
(setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)

;; Org-Mode
(setq org-directory "~/org/")



