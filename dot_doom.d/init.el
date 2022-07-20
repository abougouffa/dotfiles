;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

;; I add some special stuff wich I want to load very early.
(load! "pseudo-early-init.el")

(doom!
  :input
  bidi

  :completion
  (vertico +icons)
  company

  :ui
  deft
  doom
  doom-dashboard
  hl-todo
  hydra
  modeline
  vc-gutter
  zen
  ophints
  nav-flash
  (window-select +numbers)
  (ligatures +extra)
  (popup +all +defaults)
  (emoji +ascii +unicode +github)
  (treemacs +lsp)
  workspaces
  ;;tabs
  ;;unicode
  ;;neotree
  ;;doom-quit
  ;;indent-guides
  ;;minimap
  ;;vi-tilde-fringe

  :editor
  (evil +everywhere)
  file-templates
  fold
  format
  multiple-cursors
  parinfer
  snippets
  word-wrap
  ;;lispy
  ;;(objed +manual)
  ;;god
  ;;rotate-text

  :emacs
  (dired +dirvish +icons)
  (ibuffer +icons)
  (undo +tree)
  vc
  ;;electric

  :term
  eshell
  vterm
  shell
  term

  :checkers
  (syntax +childframe)
  (spell +aspell)
  grammar

  :tools
  direnv
  editorconfig
  ein
  gist
  make
  pdf
  rgb
  tmux
  upload
  (lsp +peek)
  (debugger +lsp)
  (docker +lsp)
  (eval +overlay)
  (lookup +docsets +dictionary +offline)
  (magit +forge)
  tree-sitter
  ;;pass
  ;;biblio
  ;;ansible
  ;;prodigy
  ;;taskrunner
  ;;terraform

  :os
  (tty +osc)

  :lang
  plantuml
  emacs-lisp
  common-lisp
  markdown
  data
  qt
  (cc +lsp +tree-sitter)
  (json +lsp +tree-sitter)
  (julia +lsp +tree-sitter)
  (latex +lsp +latexmk +fold)
  (rust +lsp +tree-sitter)
  (ess +lsp)
  (yaml +lsp)
  (sh +lsp +tree-sitter)
  (python +lsp +pyenv +conda +pyright +tree-sitter)
  (racket +lsp +xp)
  (scheme +guile +racket +chez +gambit +gauche)
  (org +dragndrop +gnuplot +jupyter +pandoc +noter +hugo +present +pomodoro +roam2)
  (web +tree-sitter)
  ;;rst                  ; ReST in peace
  ;;(lua +lsp)           ; one-based indices? one-based indices
  ;;agda                 ; types of types of types of types...
  ;;(clojure +lsp)       ; java with a lisp
  ;;coq                  ; proofs-as-programs
  ;;crystal              ; ruby at the speed of c
  ;;csharp               ; unity, .NET, and mono shenanigans
  ;;(dart +flutter)      ; paint ui and not much else
  ;;elixir               ; erlang done right
  ;;elm                  ; care for a cup of TEA?
  ;;erlang               ; an elegant language for a more civilized age
  ;;faust                ; dsp, but you get to keep your soul
  ;;fsharp               ; ML stands for Microsoft's Language
  ;;fstar                ; (dependent) types and (monadic) effects and Z3
  ;;gdscript             ; the language you waited for
  ;;(go +lsp)            ; the hipster dialect
  ;;(haskell +dante)     ; a language that's lazier than I am
  ;;hy                   ; readability of scheme w/ speed of python
  ;;idris                ;
  ;;(java +meghanada)    ; the poster child for carpal tunnel syndrome
  ;;javascript           ; all(hope(abandon(ye(who(enter(here))))))
  ;;kotlin               ; a better, slicker Java(Script)
  ;;lean
  ;;factor
  ;;ledger               ; an accounting system in Emacs
  ;;nim                  ; python + lisp at the speed of c
  ;;nix                  ; I hereby declare "nix geht mehr!"
  ;;ocaml                ; an objective camel
  ;;php                  ; perl's insecure younger brother
  ;;purescript           ; javascript, but functional
  ;;raku                 ; the artist formerly known as perl6
  ;;rest                 ; Emacs as a REST client
  ;;(ruby +rails)        ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
  ;;scala                ; java, but good
  ;;sml
  ;;solidity             ; do you need a blockchain? No.
  ;;swift                ; who asked for emoji variables?
  ;;terra                ; Earth and Moon in alignment for performance.
  ;;web                  ; the tubes

  :email
  (mu4e +org +gmail)

  :app
  calendar
  irc
  emms
  everywhere
  (rss +org)
  ;;twitter

  :config
  literate
  (default +bindings +smartparens)
)
