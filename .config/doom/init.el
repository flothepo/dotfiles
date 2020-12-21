;;; init.el -*- lexical-binding: t; -*-

(setq doom-localleader-key ",")

(doom!
       :completion
       (company +childframe)
       (ivy +icons)

       :ui
       doom
       hl-todo
       modeline
       ophints
       (popup
        +all
        +defaults)
       treemacs
       vc-gutter
       window-select
       ;;(ligatures +fira)

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets

       :emacs
       dired
       electric
       ibuffer
       vc
       undo

       :email
       mu4e

       :tools
       (eval +overlay)
       flycheck
       (lookup
        +docsets)        
       lsp
       magit
       make
       rgb

       :lang
       cc
       common-lisp
       data
       emacs-lisp
       (haskell +lsp)
       (javascript +lsp)
       (latex +lsp)
       markdown
       ocaml
       (org
        +present)
       rust
       sh
       yaml

       :config
       literate
       (default +bindings +smartparens))
