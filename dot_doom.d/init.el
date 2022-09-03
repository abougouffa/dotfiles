;;; init.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

;; I add some special stuff wich I want to load very early.
(load! "pseudo-early-init.el")

(doom!
  :input
  bidi

  :completion
  (vertico +icons)
  (company +childframe)

  :ui
  deft
  doom
  doom-dashboard
  hl-todo
  hydra
  modeline
  zen
  ;; tabs
  ophints
  nav-flash
  (vc-gutter +diff-hl +pretty)
  (window-select +numbers)
  ;; (ligatures +extra)
  (popup +all +defaults)
  (emoji +ascii +unicode +github)
  (treemacs +lsp)
  workspaces

  :editor
  (evil +everywhere)
  file-templates
  fold
  format
  multiple-cursors
  parinfer
  snippets
  word-wrap

  :emacs
  (dired +dirvish +icons)
  (ibuffer +icons)
  undo
  vc

  :term
  eshell
  vterm
  shell
  term

  :checkers
  (syntax +childframe)
  (spell +aspell)
  (grammar +lsp)

  :tools
  ein
  pdf
  rgb
  gist
  make
  tmux
  direnv
  upload
  biblio
  tree-sitter
  editorconfig
  (lsp +peek)
  (docker +lsp)
  (magit +forge)
  (debugger +lsp)
  (eval +overlay)
  (lookup +docsets +dictionary +offline)

  :os
  (tty +osc)

  :lang
  qt
  data
  plantuml
  emacs-lisp
  common-lisp
  (ess +lsp)
  (yaml +lsp)
  (markdown +grip)
  (csharp +dotnet)
  (racket +lsp +xp)
  (lua +lsp +fennel)
  (web +tree-sitter)
  (cc +lsp +tree-sitter)
  (sh +lsp +tree-sitter)
  (json +lsp +tree-sitter)
  (rust +lsp +tree-sitter)
  (julia +lsp +tree-sitter)
  (latex +lsp +latexmk)
  (python +lsp +pyenv +pyright +tree-sitter)
  (scheme +chez +mit +chicken +gauche +guile +chibi)
  (org +dragndrop +gnuplot +jupyter +pandoc +noter +journal +hugo +present +pomodoro +roam2)

  :email
  (:if (executable-find "mu") (mu4e +org +gmail))

  :app
  calendar
  irc
  emms
  everywhere
  rss

  :config
  literate
  (default +bindings +smartparens)
)
