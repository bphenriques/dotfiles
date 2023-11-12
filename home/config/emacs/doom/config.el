;;; -*- lexical-binding: t; -*-

;;; Basic Information
(setq user-full-name "Bruno Henriques"
      user-mail-address "4727729+bphenriques@users.noreply.github.com")

(load! "+org-roam")
(load! "+scala")
(load! "custom")

;;; Defaults
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 undo-limit 80000000                              ; Raise undo-limit to 80Mb
 evil-want-fine-undo t                            ; By default while in insert all changes are one big blob. Be more granular
 inhibit-compacting-font-caches t                 ; When there are lots of glyphs, keep them in memory
 initial-major-mode 'org-mode                     ; New buffers will have org-mode enabled by default
)

;;; Theme
(setq
  menu-bar-mode -1                                          ; Remove top-bar when running in the terminal.
  doom-font (font-spec :family "monospace" :size 14)        ; Font.
  doom-theme 'doom-one                                      ; See all themes here: https://github.com/doomemacs/themes/tree/screenshots
  display-line-numbers-type t)                              ; Display line numbers.

;;; MacOS
(setq
 mac-option-key-is-meta t
 mac-right-option-modifier nil)

;;; Display list of buffers on the right/bottom when splitting windows
(setq
 evil-vsplit-window-right t
 evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)


;;; Enable basic mouse support when running in the terminal
(xterm-mouse-mode 1)

;;; Org configurations
(after! org
  (setq
   org-directory "~/org/"
   org-ellipsis "▼"                        ; Icon for collapsed items
   org-hide-emphasis-markers t             ; Hide markers
   org-startup-with-inline-images nil      ; do not inline images by default
))

(after! org-journal
 :config
 (setq
   org-journal-dir "~/workspace/journal"
   org-journal-file-format "%Y-%B Week %W.org"
   org-journal-file-header "#+TITLE: %Y-%B Week %W - Notes\n#+FILETAGS: %Y %B personal notes\n#+STARTUP: folded\n"
   org-journal-file-type 'weekly))


;;; Not Deft to quickly find my notes
(after! notdeft
  :config
  (setq notdeft-extension "org"
        notdeft-directories '("~/workspace/journal/"
                              "~/workspace/knowledge-base/org")))
