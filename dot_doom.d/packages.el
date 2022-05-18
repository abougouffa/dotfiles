;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*SVG Tag Mode][SVG Tag Mode:1]]
(package! svg-tag-mode)
;; SVG Tag Mode:1 ends here

(package! nano-doom
  :disable t
  :recipe (:host github :repo "skyler544/doom-nano-testing"))

(package! lambda-themes
  :recipe (:host github
           :repo "Lambda-Emacs/lambda-themes"))

(package! lambda-line
  :recipe (:host github
           :repo "Lambda-Emacs/lambda-line"))

(package! humanoid-themes
  :recipe (:host github
           :repo "humanoid-colors/emacs-humanoid-themes"))

(package! focus)

(when (<= emacs-major-version 28)
  (package! good-scroll))

;; [[file:config.org::*Evil][Evil:1]]
(package! evil-escape :disable t)
;; Evil:1 ends here

(package! aggressive-indent)

(unpin! pdf-tools)

;; lisp/wttrin/wttrin.el taken from:
;; https://raw.githubusercontent.com/tecosaur/emacs-config/master/lisp/wttrin/wttrin.el
(package! wttrin
  :recipe (:local-repo "lisp/wttrin")
  :ignore t)

(package! osm)

(package! awqat
  :recipe (:host github
           :repo "zkry/awqat"))

(package! ess-view)

(package! vlf)

(package! info-colors)

(package! guess-language
  :recipe (:host github
           :repo "tmalsburg/guess-language.el"))

(package! grammarly
  :recipe (:host github
           :repo "emacs-grammarly/grammarly"))

;; Install the suitable LSP frontend
(if (featurep! :tools lsp +eglot)
  (package! eglot-grammarly
    :recipe (:host github
             :repo "emacs-grammarly/eglot-grammarly"))
  (package! lsp-grammarly
    :recipe (:host github
             :repo "emacs-grammarly/lsp-grammarly")))

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

(package! lemon
  :recipe (:host gitlab
           :repo "ieure/lemon"))

(package! bitwarden
  :recipe (:host github
           :repo "seanfarley/emacs-bitwarden"))

(package! speed-type)

(package! 2048-game)

(package! snow)

(package! xkcd
  :recipe (:host github
           :repo "vibhavp/emacs-xkcd"))

(package! nasm-mode)
(package! haxor-mode)
(package! mips-mode)
(package! riscv-mode)
(package! x86-lookup)

(package! repo)

(package! devdocs
  :recipe (:host github
           :repo "astoff/devdocs.el"
           :files ("*.el")))

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

(package! nov)

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

(unpin! dap-mode)

(package! gdb-mi
  :recipe (:host github
           :repo "weirdNox/emacs-gdb"
           :files ("*.el" "*.c" "*.h" "Makefile")))

(package! org-super-agenda)

(package! caldav
  :recipe (:host github
           :repo "dengste/org-caldav"))

(package! doct)

(package! org-appear)

(unpin! org-roam) ;; To avoid problems with org-roam-ui
(package! websocket)
(package! org-roam-ui)

(package! org-wild-notifier)

(package! org-modern
  :recipe (:host github
           :repo "minad/org-modern"))

(package! org-fragtog)

(package! org-bib
  :recipe (:host github
           :repo "rougier/org-bib-mode" ))

(package! org-ref)

(package! org-ref-cite
  :recipe (:host github
           :repo "jkitchin/org-ref-cite"
           :files (:defaults "readme.org"))
  :disable t) ;; BUG: Not working correctly!

(package! academic-phrases
  :recipe (:host github
           :repo "nashamri/academic-phrases"))
