;; -*- no-byte-compile: t; -*-

(package! svg-tag-mode)

(package! nano-doom
  :disable t
  :recipe (:host github :repo "skyler544/doom-nano-testing"))

(package! bespoke-themes
  :recipe (:host github
           :repo "mclear-tools/bespoke-themes"))

(package! bespoke-modeline
  :recipe (:host github
           :repo "mclear-tools/bespoke-modeline"))

(package! focus)

(package! goggles
  :recipe (:host github
           :repo "minad/goggles"))

(when (<= emacs-major-version 28)
  (package! good-scroll))

;; lisp/wttrin/wttrin.el taken from:
;; https://raw.githubusercontent.com/tecosaur/emacs-config/master/lisp/wttrin/wttrin.el
(package! wttrin
  :recipe (:local-repo "lisp/wttrin"))

(package! osm)

(package! awqat
  :recipe (:host github
           ;; Temporary use my fork with angles fixes.
           ;; Upstream repo "zkry/awqat"
           :repo "abougouffa/awqat"
           :branch "fix-angle-for-uoif"))

(package! ess-view)

(package! vlf)

(package! nov)

(package! info-colors)

(package! guess-language
  :recipe (:host github
           :repo "tmalsburg/guess-language.el"))

(package! grammarly
  :recipe (:host github
           :repo "emacs-grammarly/grammarly"))

(package! lsp-grammarly
  :recipe (:host github
           :repo "emacs-grammarly/lsp-grammarly"))

(package! flycheck-grammalecte
  :recipe (:host github
           :repo "milouse/flycheck-grammalecte"))

(package! zotxt)

(package! crdt)

(package! ag)

(package! disk-usage)

(package! aweshell
  :recipe (:host github
           :repo "manateelazycat/aweshell"))

(package! page-break-lines)

(package! popweb
  :recipe (:host github
           :repo "manateelazycat/popweb"
           :files ("*.el" "*.js" "*.py" "extension"))
  :disable t) ; It works, but I'm not using it ATM

(package! chezmoi)

(package! speed-type)

(package! 2048-game)

(package! snow)

(package! xkcd
  :recipe (:host github
           :repo "vibhavp/emacs-xkcd"))

(package! doct)

(package! org-super-agenda)

(package! caldav
  :recipe (:host github
           :repo "dengste/org-caldav"))

(package! academic-phrases
  :recipe (:host github
           :repo "nashamri/academic-phrases"))

(package! org-bib
  :recipe (:host github
           :repo "rougier/org-bib-mode" ))

(package! org-ref)

(package! org-ref-cite
  :recipe (:host github
           :repo "jkitchin/org-ref-cite"
           :files (:defaults "readme.org"))
  :disable t) ;; BUG: Not working correctly!

(package! org-fragtog)

(package! org-pretty-table
  :recipe (:host github
           :repo "Fuco1/org-pretty-table"))

(package! org-modern
  :recipe (:host github
           :repo "minad/org-modern")
  :disable t) ;; Not working ATM

(unpin! org-roam) ;; To avoid problems with org-roam-ui
(package! websocket)
(package! org-roam-ui)

(package! org-wild-notifier)

(package! nasm-mode)
(package! haxor-mode)
(package! mips-mode)
(package! x86-lookup)

(package! repo)

(package! devdocs
  :recipe (:host github
           :repo "astoff/devdocs.el"
           :files ("*.el")))

(package! gdb-mi
  :recipe (:host github
           :repo "weirdNox/emacs-gdb"
           :files ("*.el" "*.c" "*.h" "Makefile")))

(package! embed
  :recipe (:host github
           :repo "sjsch/embed-el"))

(package! disaster)

(package! magit-delta
  :disable t) ;; Disabled, too slow on big chunks

(package! blamer
  :recipe (:host github
           :repo "artawower/blamer.el"))

(package! systemd)

(package! bitbake-modes
  :recipe (:host bitbucket
           :repo "olanilsson/bitbake-modes"))

(package! aas
  :recipe (:host github
           :repo "ymarco/auto-activating-snippets"))

(package! project-cmake
  :disable (not (featurep! :tools lsp +eglot)) ; Enable only if (lsp +eglot) is used
  :recipe (:host github
           :repo "juanjosegarciaripoll/project-cmake"))

(package! franca-idl
  :recipe (:host github
           :repo "zeph1e/franca-idl.el"))

(package! flycheck-projectile
  :recipe (:host github
           :repo "nbfalcon/flycheck-projectile"))

(package! graphviz-dot-mode)

(package! imaxima)
(package! maxima
  :recipe (:host gitlab
           :repo "sasanidas/maxima"
           :files ("*.el" "keywords")))

(package! rosemacs
  :recipe (:host github
           :repo "code-iai/ros_emacs_utils"
           :files ("rosemacs/*"))
  :disable t) ;; No clear way to make it work on a remote machine

;; `ros.el' depends on `with-shell-interpreter' among other packages
;; See: https://github.com/DerBeutlin/ros.el/blob/master/Cask
(package! with-shell-interpreter)
(package! ros
  :recipe (:host github
           :repo "DerBeutlin/ros.el"))

(package! unibeautify
  :recipe (:host github
           :repo "Unibeautify/emacs"))

(package! inspector
  :recipe (:host github
           :repo "mmontone/emacs-inspector"))

(unpin! dap-mode)
