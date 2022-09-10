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
  zen
  deft
  doom
  hydra
  hl-todo
  ophints
  modeline
  nav-flash
  workspaces
  indent-guides
  doom-dashboard
  (treemacs +lsp)
  (ligatures +extra)
  (popup +all +defaults)
  (emoji +ascii +github)
  (window-select +numbers)
  (vc-gutter +diff-hl +pretty)

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
  vc
  undo
  (ibuffer +icons)

  :term
  term
  vterm
  shell
  eshell

  :checkers
  (syntax +childframe)
  (spell +aspell)

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
  (latex +lsp +latexmk)
  (cc +lsp +tree-sitter)
  (sh +lsp +tree-sitter)
  (json +lsp +tree-sitter)
  (rust +lsp +tree-sitter)
  (julia +lsp +tree-sitter)
  (python +lsp +pyenv +pyright +tree-sitter)
  (scheme +chez +mit +chicken +gauche +guile +chibi)
  (org +dragndrop +gnuplot +jupyter +pandoc +noter +journal +hugo +present +pomodoro +roam2)

  :email
  (:if (executable-find "mu") (mu4e +org +gmail))

  :app
  irc
  rss
  emms
  calendar
  everywhere

  :config
  literate
  (default +bindings +smartparens)

  :private
  (grammar +lsp)
  (dired-ng +icons +bindings)
)
