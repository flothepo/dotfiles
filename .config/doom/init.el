;;; init.el -*- lexical-binding: t; -*-

(setq doom-localleader-key ",")

(doom!
 :completion
 (company +childframe)
 (ivy
  +prescient
  +icons)

 :ui
 doom
 hl-todo
 modeline
 ophints
 (emoji +ascii +unicode)
 (popup
  +all
  +defaults)
 treemacs
 vc-gutter
 window-select

 :editor
 (evil +everywhere)
 file-templates
 fold
 (format +onsave)
 parinfer
 snippets
 lispy

 :emacs
 (dired +icons)
 electric
 vc
 (undo +tree)

 :email
 mu4e

 :checkers
 (syntax +childframe)
 (spell +flyspell +hunspell)

 :term
 eshell
 vterm

 :tools
 (eval +overlay)
 (docker +lsp)
 (debugger +lsp)
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
 json
 (latex
  +lsp
  +latexmk
  +fold)
 markdown
 (ocaml +lsp)
 (org
  +present)
 (python +lsp)
 rest
 (rust +lsp)
 sh
 web
 yaml

 :config
 literate
 (default +bindings +smartparens))
