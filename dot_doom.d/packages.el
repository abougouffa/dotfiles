;; [[file:config.org::*Additional packages (=packages.el=)][Additional packages (=packages.el=):1]]
;; -*- no-byte-compile: t; -*-
;; Additional packages (=packages.el=):1 ends here

;; [[file:config.org::*SVG tag][SVG tag:1]]
(package! svg-tag-mode)
;; SVG tag:1 ends here

;; [[file:config.org::*Focus][Focus:1]]
(package! focus)
;; Focus:1 ends here

;; [[file:config.org::*Smooth scrolling][Smooth scrolling:1]]
(when (<= emacs-major-version 28)
  (package! good-scroll))
;; Smooth scrolling:1 ends here

;; [[file:config.org::*Page break lines][Page break lines:1]]
(package! page-break-lines)
;; Page break lines:1 ends here

;; [[file:config.org::*Very large files][Very large files:1]]
(package! vlf)
;; Very large files:1 ends here

;; [[file:config.org::*Evil][Evil:1]]
(package! evil-escape :disable t)
;; Evil:1 ends here

;; [[file:config.org::*Aggressive indent][Aggressive indent:1]]
(package! aggressive-indent)
;; Aggressive indent:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(unpin! treemacs)
(unpin! lsp-treemacs)
;; Treemacs:1 ends here

;; [[file:config.org::*SonarLint][SonarLint:1]]
(package! lsp-sonarlint)
;; SonarLint:1 ends here

;; [[file:config.org::*Project CMake][Project CMake:1]]
(package! project-cmake
  :disable (not (featurep! :tools lsp +eglot)) ; Enable only if (lsp +eglot) is used
  :recipe (:host github
           :repo "juanjosegarciaripoll/project-cmake"))
;; Project CMake:1 ends here

;; [[file:config.org::*Clang-format][Clang-format:1]]
(package! clang-format)
;; Clang-format:1 ends here

;; [[file:config.org::*Auto-include C++ headers][Auto-include C++ headers:1]]
(package! cpp-auto-include
  :recipe (:host github
           :repo "emacsorphanage/cpp-auto-include"))
;; Auto-include C++ headers:1 ends here

;; [[file:config.org::*Emacs Refactor][Emacs Refactor:1]]
(package! erefactor
  :recipe (:host github
           :repo "mhayashi1120/Emacs-erefactor"))
;; Emacs Refactor:1 ends here

;; [[file:config.org::*Guess language][Guess language:1]]
(package! guess-language
  :recipe (:host github
           :repo "tmalsburg/guess-language.el"))
;; Guess language:1 ends here

;; [[file:config.org::*Grammarly][Grammarly:1]]
(package! grammarly
  :recipe (:host github
           :repo "emacs-grammarly/grammarly"))
;; Grammarly:1 ends here

;; [[file:config.org::*Eglot][Eglot:1]]
(package! eglot-grammarly
  :disable (not (featurep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-grammarly/eglot-grammarly"))
;; Eglot:1 ends here

;; [[file:config.org::*LSP Mode][LSP Mode:1]]
(package! lsp-grammarly
  :disable (or (not (featurep! :tools lsp)) (featurep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-grammarly/lsp-grammarly"))
;; LSP Mode:1 ends here

;; [[file:config.org::*Grammalecte][Grammalecte:1]]
(package! flycheck-grammalecte
  :recipe (:host github
           :repo "milouse/flycheck-grammalecte"))
;; Grammalecte:1 ends here

;; [[file:config.org::*LTeX][LTeX:1]]
;; Needed, but not installed automatically
(package! github-tags
  :recipe (:host github
           :repo "jcs-elpa/github-tags"))

(package! lsp-ltex
  :disable (and (featurep! :tools lsp) (featurep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-languagetool/lsp-ltex"))

(package! eglot-ltex
  :disable (not (featurep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-languagetool/eglot-ltex"))
;; LTeX:1 ends here

;; [[file:config.org::*Flycheck][Flycheck:1]]
(package! flycheck-languagetool
  :disable t ;; Disabled, using LTeX LSP
  :recipe (:host github
           :repo "emacs-languagetool/flycheck-languagetool"))
;; Flycheck:1 ends here

;; [[file:config.org::*Disk usage][Disk usage:1]]
(package! disk-usage)
;; Disk usage:1 ends here

;; [[file:config.org::*Chezmoi][Chezmoi:1]]
(package! chezmoi)
;; Chezmoi:1 ends here

;; [[file:config.org::*Aweshell][Aweshell:1]]
(package! aweshell
  :recipe (:host github
           :repo "manateelazycat/aweshell"))
;; Aweshell:1 ends here

;; [[file:config.org::*Lemon][Lemon:1]]
(package! lemon
  :recipe (:host nil
           :repo "https://codeberg.org/emacs-weirdware/lemon.git"))
;; Lemon:1 ends here

;; [[file:config.org::*Weather][Weather:1]]
;; lisp/wttrin/wttrin.el taken from:
;; https://raw.githubusercontent.com/tecosaur/emacs-config/master/lisp/wttrin/wttrin.el
(package! wttrin
  :recipe (:local-repo "lisp/wttrin"))
;; Weather:1 ends here

;; [[file:config.org::*OpenStreetMap][OpenStreetMap:1]]
(package! osm)
;; OpenStreetMap:1 ends here

;; [[file:config.org::*Islamic prayer times][Islamic prayer times:1]]
(package! awqat
  :recipe (:host github
           :repo "zkry/awqat"))
;; Islamic prayer times:1 ends here

;; [[file:config.org::*Info colors][Info colors:1]]
(package! info-colors)
;; Info colors:1 ends here

;; [[file:config.org::*Zotero Zotxt][Zotero Zotxt:1]]
(package! zotxt)
;; Zotero Zotxt:1 ends here

;; [[file:config.org::*CRDT][CRDT:1]]
(package! crdt)
;; CRDT:1 ends here

;; [[file:config.org::*The Silver Searcher][The Silver Searcher:1]]
(package! ag)
;; The Silver Searcher:1 ends here

;; [[file:config.org::*Bitwarden][Bitwarden:1]]
(package! bitwarden
  :recipe (:host github
           :repo "seanfarley/emacs-bitwarden"))
;; Bitwarden:1 ends here

;; [[file:config.org::*LTDR][LTDR:1]]
(package! tldr)
;; LTDR:1 ends here

;; [[file:config.org::*FZF][FZF:1]]
(package! fzf)
;; FZF:1 ends here

;; [[file:config.org::*Speed Type][Speed Type:1]]
(package! speed-type)
;; Speed Type:1 ends here

;; [[file:config.org::*2048 Game][2048 Game:1]]
(package! 2048-game)
;; 2048 Game:1 ends here

;; [[file:config.org::*Snow][Snow:1]]
(package! snow)
;; Snow:1 ends here

;; [[file:config.org::*=xkcd=][=xkcd=:1]]
(package! xkcd
  :recipe (:host github
           :repo "vibhavp/emacs-xkcd"))
;; =xkcd=:1 ends here

;; [[file:config.org::*e-Books =nov=][e-Books =nov=:1]]
(package! nov)
;; e-Books =nov=:1 ends here

;; [[file:config.org::*Cycle song information in mode line][Cycle song information in mode line:1]]
(package! emms-mode-line-cycle
  :recipe (:host github
           :repo "abougouffa/emms-mode-line-cycle"))
;; Cycle song information in mode line:1 ends here

;; [[file:config.org::*Maxima][Maxima:1]]
(package! maxima
  :recipe (:host github
           :repo "emacsmirror/maxima"
           :files (:defaults
                   "keywords"
                   "company-maxima.el"
                   "poly-maxima.el")))
;; Maxima:1 ends here

;; [[file:config.org::*IMaxima][IMaxima:1]]
;; Use the `imaxima' package bundled with the official Maxima distribution.
(package! imaxima
  :recipe (:host nil ;; Unsupported host, we will specify the complete repo link
           :repo "https://git.code.sf.net/p/maxima/code"
           :files ("interfaces/emacs/imaxima/*")))
;; IMaxima:1 ends here

;; [[file:config.org::*Vim][Vim:1]]
(package! vimrc-mode
  :recipe (:host github
           :repo "mcandre/vimrc-mode"))
;; Vim:1 ends here

;; [[file:config.org::*ESS][ESS:1]]
(package! ess-view)
;; ESS:1 ends here

;; [[file:config.org::*=ros.el=][=ros.el=:1]]
;; `ros.el' depends on `with-shell-interpreter' among other packages
;; See: https://github.com/DerBeutlin/ros.el/blob/master/Cask
(package! with-shell-interpreter)
(package! ros
  :recipe (:host github
           :repo "DerBeutlin/ros.el"))
;; =ros.el=:1 ends here

;; [[file:config.org::*Embed.el][Embed.el:1]]
(package! embed
  :recipe (:host github
           :repo "sjsch/embed-el"))
;; Embed.el:1 ends here

;; [[file:config.org::*Arduino][Arduino:1]]
(package! arduino-mode
  :recipe (:host github
           :repo "bookest/arduino-mode"))
;; Arduino:1 ends here

;; [[file:config.org::*Bitbake (Yocto)][Bitbake (Yocto):1]]
(package! bitbake-modes
  :recipe (:host bitbucket
           :repo "olanilsson/bitbake-modes"))
;; Bitbake (Yocto):1 ends here

;; [[file:config.org::*DAP][DAP:1]]
(unpin! dap-mode)
;; DAP:1 ends here

;; [[file:config.org::*Emacs GDB][Emacs GDB:1]]
(package! gdb-mi
  :recipe (:host github
           :repo "weirdNox/emacs-gdb"
           :files ("*.el" "*.c" "*.h" "Makefile")))
;; Emacs GDB:1 ends here

;; [[file:config.org::*Valgrind][Valgrind:1]]
(package! valgrind
  :recipe (:local-repo "lisp/valgrind"))
;; Valgrind:1 ends here

;; [[file:config.org::*WIP Company for commit messages][WIP Company for commit messages:1]]
(package! company-gitcommit
  :recipe (:local-repo "lisp/company-gitcommit"))
;; WIP Company for commit messages:1 ends here

;; [[file:config.org::*Repo][Repo:1]]
(package! repo)
;; Repo:1 ends here

;; [[file:config.org::*Blamer][Blamer:1]]
(package! blamer
  :recipe (:host github
           :repo "artawower/blamer.el"))
;; Blamer:1 ends here

;; [[file:config.org::*Assembly][Assembly:1]]
(package! nasm-mode)
(package! haxor-mode)
(package! mips-mode)
(package! riscv-mode)
(package! x86-lookup)
;; Assembly:1 ends here

;; [[file:config.org::*Disaster][Disaster:1]]
(package! disaster
  :pin "9662437834e79cda3c468952ec03947aa39e92f6"
  :recipe (:host github
           :repo "abougouffa/disaster"))
;; Disaster:1 ends here

;; [[file:config.org::*Devdocs][Devdocs:1]]
(package! devdocs
  :recipe (:host github
           :repo "astoff/devdocs.el"
           :files ("*.el")))
;; Devdocs:1 ends here

;; [[file:config.org::*Systemd][Systemd:1]]
(package! systemd)
;; Systemd:1 ends here

;; [[file:config.org::*Franca IDL][Franca IDL:1]]
(package! franca-idl
  :recipe (:host github
           :repo "zeph1e/franca-idl.el"))
;; Franca IDL:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
(package! aas
  :recipe (:host github
           :repo "ymarco/auto-activating-snippets"))
;; LaTeX:1 ends here

;; [[file:config.org::*Flycheck + Projectile][Flycheck + Projectile:1]]
(package! flycheck-projectile
  :recipe (:host github
           :repo "nbfalcon/flycheck-projectile"))
;; Flycheck + Projectile:1 ends here

;; [[file:config.org::*Graphviz][Graphviz:1]]
(package! graphviz-dot-mode)
;; Graphviz:1 ends here

;; [[file:config.org::*Mermaid][Mermaid:1]]
(package! mermaid-mode)

(package! ob-mermaid
  :recipe (:host github
           :repo "arnm/ob-mermaid"))
;; Mermaid:1 ends here

;; [[file:config.org::*Inspector][Inspector:1]]
(package! inspector
  :recipe (:host github
           :repo "mmontone/emacs-inspector"))
;; Inspector:1 ends here

;; [[file:config.org::*Org mode additional packages][Org mode additional packages:1]]
(unpin! org-roam) ;; To avoid problems with org-roam-ui
(package! websocket)
(package! org-roam-ui)
(package! org-wild-notifier)
(package! org-fragtog)
(package! org-ref)
(package! org-appear)
(package! org-super-agenda)
(package! doct)

(package! org-mode
  ;; https://github.com/doomemacs/doomemacs/issues/6478#issuecomment-1160699339
  :pin "971eb6885ec996c923e955730df3bafbdc244e54")

(package! caldav
  :recipe (:host github
           :repo "dengste/org-caldav"))

(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(package! org-modern
  :recipe (:host github
           :repo "minad/org-modern"))

(package! org-bib
  :recipe (:host github
           :repo "rougier/org-bib-mode"))

(package! academic-phrases
  :recipe (:host github
           :repo "nashamri/academic-phrases"))
;; Org mode additional packages:1 ends here

;; [[file:config.org::*Quarto][Quarto:1]]
(package! quarto-mode)
;; Quarto:1 ends here
