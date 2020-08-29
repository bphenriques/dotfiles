;;; ../../Documents/repos/dotfiles/emacs/.config/doom/+scala.el -*- lexical-binding: t; -*-

(require 'ob-ammonite)

; .ammonite/.ammonite/predef.sc's has to include repl.prompt() = "scala>"
(setq ob-ammonite-prompt-str "scala>")

; Provide scala alias for the amm as it is the real name and allows syntax highlight when exporting
(defun org-babel-execute:scala (body params)
  "Execute the scala code in BODY using PARAMS using org-babel-execute:amm"
  (org-babel-execute:amm body params))
