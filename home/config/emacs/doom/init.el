;;; init.el -*- lexical-binding: t; -*-

;;; Docs:
;;; - Full list: https://github.com/doomemacs/doomemacs/blob/master/docs/modules.org
;;; - Press 'SPC h d h' to access Doom's documentation.
;;; - Full file: https://github.com/doomemacs/doomemacs/blob/master/templates/init.example.el
;;; Run 'doom sync' afterwards
(doom! :input
       :completion
       company
       (vertico +icons)

       :ui
       doom                         ; what makes DOOM look the way it does
       hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline                     ; snazzy, Atom-inspired modeline, plus API
       nav-flash                    ; blink cursor line after big motions
       neotree                      ; a project drawer, like NERDTree for vim
       ophints                      ; highlight the region an operation acts on
       (popup +defaults)            ; tame sudden yet inevitable temporary windows
       tabs                         ; a tab bar for Emacs
       (treemacs +lsp)              ; a project drawer, like neotree but cooler
       unicode                      ; extended unicode support for various languages
       (vc-gutter +pretty)          ; vcs diff in the fringe
       vi-tilde-fringe              ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       zen                          ; distraction-free coding or writing

       :editor
       (evil +everywhere) ; come to the dark side, we have cookies
       fold               ; (nigh) universal code folding
       format             ; automated prettiness
       multiple-cursors   ; editing in many places at once
       snippets           ; my elves. They type so I don't have to

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes

       :term
       :checkers
       syntax                         ; tasing you for every semicolon you forget
       (spell +hunspell +everywhere)  ; Same spell checker as Mozilla

       :tools
       (debugger +lsp)
       direnv
       (eval +overlay)                  ; run code, run (also, repls)
       (lookup +dictionary +offline)    ; navigate your code and its documentation
       (lsp +peek)                      ; Language Server Protocol
       magit                            ; a git porcelain for Emacs
       ;;make                           ; run make tasks from Emacs
       ;;pdf                            ; pdf enhancements
       rgb                              ; creating color strings
       terraform                        ; infrastructure as code
       tree-sitter                      ; syntax and parsing, sitting in a tree...

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty                 ; improve the terminal Emacs experience

       :lang
       emacs-lisp
       (json +tree-sitter)
       (java +lsp)
       (kotlin +lsp)
       (latex +lsp)
       (markdown +grip)
       (nix +tree-sitter)
       (org +hugo +roam2 +dragndrop +journal +present)
       (rust +tree-sitter)
       (scala +lsp +tree-sitter)
       (sh +tree-sitter +lsp)
       web
       (yaml +lsp +tree-sitter)

       :email
       :app
       :config
       (default +bindings +smartparens))
