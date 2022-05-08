;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :input
       bidi

       :completion
       (vertico +icons)             ; the search engine of the future
       company                      ; the ultimate code completion backend
       ;;(ivy +childframe           ; a search engine for love and life
       ;;     +fuzzy
       ;;     +icons
       ;;     +prescient)
       ;;helm                       ; the *other* search engine for love and life
       ;;ido                        ; the other *other* search engine...

       :ui
       deft                         ; notational velocity for Emacs
       doom                         ; what makes DOOM look the way it does
       doom-dashboard               ; a nifty splash screen for Emacs
       hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra                        ; quick documentation for related commands
       modeline                     ; snazzy, Atom-inspired modeline, plus API
       nav-flash                    ; blink the current line after jumping
       ophints                      ; highlight the region an operation acts on
       vc-gutter                    ; vcs diff in the fringe
       workspaces                   ; tab emulation, persistence & separate workspaces
       zen                          ; distraction-free coding or writing
       (window-select +numbers)     ; visually switch windows
       (ligatures +extra)           ; ligatures and symbols to make your code pretty again
       ;;(treemacs +lsp)            ; a project drawer, like neotree but cooler
       (popup +all                  ; tame sudden yet inevitable temporary windows
              +defaults)
       (emoji +ascii
              +unicode
              +github)
       ;;doom-quit                  ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column                ; a `fill-column' indicator
       ;;indent-guides              ; highlighted indent columns, notoriously slow
       ;;minimap                    ; show a map of the code on the side
       neotree                      ; a project drawer, like NERDTree for vim
       ;;unicode                    ; extended unicode support for various languages
       ;;tabs                       ; a tab bar for Emacs
       ;;vi-tilde-fringe            ; fringe tildes to mark beyond EOB

       :editor
       (evil +everywhere)           ; come to the dark side, we have cookies
       (objed +manual)              ; text object editing for the innocent
       file-templates               ; auto-snippets for empty files
       fold                         ; (nigh) universal code folding
       format                       ; automated prettiness
       multiple-cursors             ; editing in many places at once
       parinfer                     ; turn lisp into python, sort of
       snippets                     ; my elves. They type, so I don't have to
       word-wrap                    ; soft wrapping with language-aware indent
       lispy                        ; vim for lisp, for people who don't like vim
       ;;god                        ; run Emacs commands without modifier keys
       ;;rotate-text                ; cycle region at point between text candidates

       :emacs
       (dired +icons                ; making dired pretty [functional]
              +ranger)
       (ibuffer +icons)             ; interactive buffer management
       (undo +tree)                 ; persistent, smarter undo for your inevitable mistakes
       electric                     ; smarter, keyword-based electric-indent
       vc                           ; version-control and Emacs, sitting in a tree

       :term
       eshell                       ; the elisp shell that works everywhere
       vterm                        ; the best terminal emulation in Emacs
       shell                        ; simple shell REPL for Emacs
       term                         ; basic terminal emulator for Emacs

       :checkers
       (syntax +childframe)   ; tasing you for every semicolon you forget
       (spell +aspell)
       grammar                ; tasing grammar mistake every you make

       :tools
       direnv
       editorconfig           ; let someone else argue about tabs vs spaces
       ein                    ; tame Jupyter notebooks with emacs
       ;;chezmoi
       ;;biblio               ; Writes a PhD for you (citation needed)
       gist                   ; interacting with github gists
       make                   ; run make tasks from Emacs
       pdf                    ; pdf enhancements
       rgb                    ; creating color strings
       tmux                   ; an API for interacting with tmux
       upload                 ; map local to remote projects via ssh/ftp
       (lsp +peek)            ; LPS
       (debugger +lsp)        ; FIXME stepping through code, to help you add bugs
       (docker +lsp)
       (eval +overlay)        ; run code, run (also, repls)
       (lookup +docsets       ; navigate your code and its documentation
               +dictrionary
               +offline)
       (magit +forge)         ; a git porcelain for Emacs
       ;;ansible
       ;;pass                 ; password manager for nerds
       ;;prodigy              ; FIXME managing external services & code builders
       ;;taskrunner           ; taskrunner for all your projects
       ;;terraform            ; infrastructure as code

       :os
       (tty +osc)             ; Configures Emacs for use in the terminal

       :lang
       plantuml               ; diagrams for confusing people more
       emacs-lisp             ; drown in parentheses
       common-lisp            ; if you've seen one lisp, you've seen them all
       markdown               ; writing docs for people to ignore
       data                   ; config/data formats
       qt                     ; the 'cutest' gui framework ever
       (cc +lsp)              ; C/C++/Obj-C madness
       (json +lsp)            ; At least it ain't XML
       (julia +lsp)           ; a better, faster MATLAB
       (latex +lsp            ; writing papers in Emacs has never been so fun
              +latexmk
              +cdlatex
              +fold)
       (rust +lsp)            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (ess +lsp)             ; emacs speaks statistics
       (yaml +lsp)            ; JSON, but readable
       (sh +lsp)              ; she sells {ba,z,fi}sh shells on the C xor
       (python +lsp           ; beautiful is better than ugly
               +pyright
               +pyenv
               +conda)
       (org +dragndrop        ; organize your plain life in plain text
            +gnuplot
            +jupyter
            +pandoc
            +noter
            +hugo
            +present
            +pomodoro
            +roam2
            +pretty)
       (racket +lsp           ; a DSL for DSLs
               +xp)
       (scheme +mit           ; a fully conniving family of lisps
               +guile
               +racket
               +chez)
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
       (mu4e +org
             +gmail)
       ;;(notmuch +org
       ;;         +afew)
       ;;(wanderlust +gmail)

       :app
       calendar
       irc                    ; how neckbeards socialize
       emms
       everywhere             ; *leave* Emacs!? You must be joking
       (rss +org)             ; emacs as an RSS reader
       ;;twitter              ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings
                +smartparens)
)
