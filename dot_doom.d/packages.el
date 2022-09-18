;; [[file:config.org::*Additional packages (=packages.el=)][Additional packages (=packages.el=):1]]
;; -*- coding: utf-8-unix; no-byte-compile: t; -*-
;; Additional packages (=packages.el=):1 ends here

;; [[file:config.org::*Auto-save][Auto-save:1]]
(package! super-save
  :disable t
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")
;; Auto-save:1 ends here

;; [[file:config.org::*Visual undo (=vundo=)][Visual undo (=vundo=):1]]
(package! vundo
  :recipe (:host github
           :repo "casouri/vundo")
  :pin "16a09774ddfbd120d625cdd35fcf480e76e278bb")
;; Visual undo (=vundo=):1 ends here

;; [[file:config.org::*Modus themes][Modus themes:1]]
(package! modus-themes :pin "e4ee971f53d5c687de7bfbcf9fd1522bb42be902")
;; Modus themes:1 ends here

;; [[file:config.org::*Ef (εὖ) themes][Ef (εὖ) themes:1]]
(package! ef-themes :pin "ae2bc74164d4355b8423f5ad3e5869d94556d963")
;; Ef (εὖ) themes:1 ends here

;; [[file:config.org::*Lambda themes][Lambda themes:1]]
(package! lambda-themes
  :disable t
  :recipe (:host github
           :repo "Lambda-Emacs/lambda-themes")
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")

(package! lambda-line
  :disable t
  :recipe (:host github
           :repo "Lambda-Emacs/lambda-line"))
;; Lambda themes:1 ends here

;; [[file:config.org::*SVG tag and =svg-lib=][SVG tag and =svg-lib=:1]]
(package! svg-tag-mode :pin "efd22edf650fb25e665269ba9fed7ccad0771a2f")
;; SVG tag and =svg-lib=:1 ends here

;; [[file:config.org::*Focus][Focus:1]]
(package! focus :pin "9dd85fc474bbc1ebf22c287752c960394fcd465a")
;; Focus:1 ends here

;; [[file:config.org::*Scrolling][Scrolling:1]]
(package! good-scroll
  :disable EMACS29+
  :pin "a7ffd5c0e5935cebd545a0570f64949077f71ee3")
;; Scrolling:1 ends here

;; [[file:config.org::*Very large files][Very large files:1]]
(package! vlf :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c")
;; Very large files:1 ends here

;; [[file:config.org::*Evil][Evil:2]]
(package! evil-escape :disable t)
;; Evil:2 ends here

;; [[file:config.org::*Aggressive indent][Aggressive indent:1]]
(package! aggressive-indent :pin "70b3f0add29faff41e480e82930a231d88ee9ca7")
;; Aggressive indent:1 ends here

;; [[file:config.org::*Unpin package][Unpin package:1]]
(unpin! lsp-mode)
;; Unpin package:1 ends here

;; [[file:config.org::*SonarLint][SonarLint:1]]
(package! lsp-sonarlint
  :disable t :pin "3313f38ed7d23947992e19f1e464c6d544124144")
;; SonarLint:1 ends here

;; [[file:config.org::*Project CMake][Project CMake:1]]
(package! project-cmake
  :disable t ; (not (modulep! :tools lsp +eglot)) ; Enable only if (lsp +eglot) is used
  :pin "3313f38ed7d23947992e19f1e464c6d544124144"
  :recipe (:host github
           :repo "juanjosegarciaripoll/project-cmake"))
;; Project CMake:1 ends here

;; [[file:config.org::*Clang-format][Clang-format:1]]
(package! clang-format :pin "e48ff8ae18dc7ab6118c1f6752deb48cb1fc83ac")
;; Clang-format:1 ends here

;; [[file:config.org::*Auto-include C++ headers][Auto-include C++ headers:1]]
(package! cpp-auto-include
  :recipe (:host github
           :repo "emacsorphanage/cpp-auto-include")
  :pin "0ce829f27d466c083e78b9fe210dcfa61fb417f4")
;; Auto-include C++ headers:1 ends here

;; [[file:config.org::*Erefactor][Erefactor:1]]
(package! erefactor
  :recipe (:host github
           :repo "mhayashi1120/Emacs-erefactor")
  :pin "bfe27a1b8c7cac0fe054e76113e941efa3775fe8")
;; Erefactor:1 ends here

;; [[file:config.org::*Lorem ipsum][Lorem ipsum:1]]
(package! emacs-lorem-ipsum
  :recipe (:host github
           :repo "jschaf/emacs-lorem-ipsum")
  :pin "da75c155da327c7a7aedb80f5cfe409984787049")
;; Lorem ipsum:1 ends here

;; [[file:config.org::*Coverage test][Coverage test:1]]
(package! cov :pin "cd3e1995c596cc227124db9537792d8329ffb696")
;; Coverage test:1 ends here

;; [[file:config.org::*DAP][DAP:1]]
(unpin! dap-mode)
;; DAP:1 ends here

;; [[file:config.org::*Additional debuggers for RealGUD][Additional debuggers for RealGUD:1]]
(package! realgud-lldb :pin "19a2c0a8b228af543338f3a8e51141a9e23484a5")
(package! realgud-ipdb :pin "f18f907aa4ddd3e59dc19ca296d4ee2dc5e436b0")

(package! realgud-trepan-xpy
  :recipe (:host github
           :repo "realgud/trepan-xpy")
  :pin "f53fea61a86226dcf5222b2814a549c8f8b8d5a9")

(package! realgud-maxima
  :recipe (:host github
           :repo "realgud/realgud-maxima")
  :pin "74d1615be9105d7f8a8d6d0b9f6d7a91638def11")
;; Additional debuggers for RealGUD:1 ends here

;; [[file:config.org::*Emacs GDB /a.k.a./ =gdb-mi=][Emacs GDB /a.k.a./ =gdb-mi=:1]]
(package! gdb-mi
  :disable t
  :recipe (:host github
           :repo "weirdNox/emacs-gdb"
           :files ("*.el" "*.c" "*.h" "Makefile"))
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")
;; Emacs GDB /a.k.a./ =gdb-mi=:1 ends here

;; [[file:config.org::*Valgrind][Valgrind:1]]
(package! valgrind
  :recipe `(:local-repo ,(expand-file-name "lisp/valgrind" doom-user-dir)))
;; Valgrind:1 ends here

;; [[file:config.org::*Grammarly][Grammarly:1]]
(package! grammarly
  :recipe (:host github
           :repo "emacs-grammarly/grammarly")
  :pin "e47b370faace9ca081db0b87ae3bcfd73212c56d")
;; Grammarly:1 ends here

;; [[file:config.org::*Eglot][Eglot:1]]
(package! eglot-grammarly
  :disable (not (modulep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-grammarly/eglot-grammarly")
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")
;; Eglot:1 ends here

;; [[file:config.org::*LSP Mode][LSP Mode:1]]
(package! lsp-grammarly
  :disable (or (not (modulep! :tools lsp)) (modulep! :tools lsp +eglot))
  :recipe (:host github
           :repo "emacs-grammarly/lsp-grammarly")
  :pin "eab5292037478c32e7d658fb5cba8b8fb6d72a7c")
;; LSP Mode:1 ends here

;; [[file:config.org::*Grammalecte][Grammalecte:1]]
(package! flycheck-grammalecte
  :recipe (:host github
           :repo "milouse/flycheck-grammalecte")
  :pin "314de13247710410f11d060a214ac4f400c02a71")
;; Grammalecte:1 ends here

;; [[file:config.org::*Go Translate (Google, Bing and DeepL)][Go Translate (Google, Bing and DeepL):1]]
(package! go-translate
  :recipe (:host github
           :repo "lorniu/go-translate")
  :pin "8bbcbce42a7139f079df3e9b9bda0def2cbb690f")
;; Go Translate (Google, Bing and DeepL):1 ends here

;; [[file:config.org::*Offline dictionaries][Offline dictionaries:1]]
(package! lexic
  :recipe (:host github
           :repo "tecosaur/lexic")
  :pin "f9b3de4d9c2dd1ce5022383e1a504b87bf7d1b09")
;; Offline dictionaries:1 ends here

;; [[file:config.org::*Disk usage][Disk usage:1]]
(package! disk-usage :pin "311542e1b5cf74aecf3df871160c5ad5d30a2579")
;; Disk usage:1 ends here

;; [[file:config.org::*Chezmoi][Chezmoi:1]]
(package! chezmoi :pin "781783c483bc8fcdba3a230bb774c3a8a5ebe396")
;; Chezmoi:1 ends here

;; [[file:config.org::*Aweshell][Aweshell:1]]
(package! aweshell
  :recipe (:host github
           :repo "manateelazycat/aweshell")
  :pin "d246df619573ca3f46070cc0ac82d024271ed243")
;; Aweshell:1 ends here

;; [[file:config.org::*Lemon][Lemon:1]]
(package! lemon
  :recipe (:host nil
           :repo "https://codeberg.org/emacs-weirdware/lemon.git")
  :pin "37a6e6d6ef0900ca19c820a2dbc122c7fe6d86cf")
;; Lemon:1 ends here

;; [[file:config.org::*Weather][Weather:1]]
(package! wttrin
  :recipe `(:local-repo ,(expand-file-name "lisp/wttrin" doom-user-dir))
  :pin "df5427ce2a5ad4dab652dbb1c4a1834d7ddc2abc")
;; Weather:1 ends here

;; [[file:config.org::*OpenStreetMap][OpenStreetMap:1]]
(package! osm :pin "808baabecd9882736b240e6ea9344047aeb669e2")
;; OpenStreetMap:1 ends here

;; [[file:config.org::*Islamic prayer times][Islamic prayer times:1]]
(package! awqat
  :recipe (:host github
           :repo "zkry/awqat")
  :pin "72b821aad0cb16165e27643c7d968e1528f00f8d")
;; Islamic prayer times:1 ends here

;; [[file:config.org::*Info colors][Info colors:1]]
(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")
;; Info colors:1 ends here

;; [[file:config.org::*Zotero Zotxt][Zotero Zotxt:1]]
(package! zotxt :pin "96a132d6b39f6bc19a58913b761d42efc198f8a4")
;; Zotero Zotxt:1 ends here

;; [[file:config.org::*CRDT][CRDT:1]]
(package! crdt :pin "ec0b9cc652c0e980d5865abbba7cbffefea6e8cc")
;; CRDT:1 ends here

;; [[file:config.org::*The Silver Searcher][The Silver Searcher:1]]
(package! ag :pin "ed7e32064f92f1315cecbfc43f120bbc7508672c")
;; The Silver Searcher:1 ends here

;; [[file:config.org::*Page break lines][Page break lines:1]]
(package! page-break-lines :pin "79eca86e0634ac68af862e15c8a236c37f446dcd")
;; Page break lines:1 ends here

;; [[file:config.org::*Bitwarden][Bitwarden:1]]
(package! bitwarden
  :recipe (:host github
           :repo "seanfarley/emacs-bitwarden")
  :pin "02d6410003a42e7fbeb4aa109aba949eea553706")
;; Bitwarden:1 ends here

;; [[file:config.org::*LTDR][LTDR:1]]
(package! tldr :pin "d3fd2a809a266c005915026799121c78e8b358f0")
;; LTDR:1 ends here

;; [[file:config.org::*FZF][FZF:1]]
(package! fzf :pin "21912ebc7e1084aa88c9d8b7715e782a3978ed23")
;; FZF:1 ends here

;; [[file:config.org::*Speed Type][Speed Type:1]]
(package! speed-type :pin "304cb8cd6c30d07577d7d864fd32858a29a73dba")
;; Speed Type:1 ends here

;; [[file:config.org::*2048 Game][2048 Game:1]]
(package! 2048-game :pin "87936ac2ff37f92daf7a69ae86ba226b031349e4")
;; 2048 Game:1 ends here

;; [[file:config.org::*Snow][Snow:1]]
(package! snow :pin "4cd41a703b730a6b59827853f06b98d91405df5a")
;; Snow:1 ends here

;; [[file:config.org::*=xkcd=][=xkcd=:1]]
(package! xkcd
  :recipe (:host github
           :repo "vibhavp/emacs-xkcd")
  :pin "80011da2e7def8f65233d4e0d790ca60d287081d")
;; =xkcd=:1 ends here

;; [[file:config.org::*e-Books (=nov=)][e-Books (=nov=):1]]
(package! nov :pin "cb5f45cbcfbcf263cdeb2d263eb15edefc8b07cb")
;; e-Books (=nov=):1 ends here

;; [[file:config.org::*EMPV][EMPV:1]]
(package! empv
  :recipe (:host github
           :repo "isamert/empv.el")
  :pin "228fd324a11da661cb471a501d04340254cc682f")
;; EMPV:1 ends here

;; [[file:config.org::*Cycle song information in mode line][Cycle song information in mode line:1]]
(package! emms-mode-line-cycle
  :recipe (:host github
           :repo "abougouffa/emms-mode-line-cycle")
  :pin "7a269c9aef9ece7ecf997f6abb9cd3818403b0bb")
;; Cycle song information in mode line:1 ends here

;; [[file:config.org::*Maxima][Maxima:1]]
(package! maxima
  :recipe (:host github
           :repo "emacsmirror/maxima"
           :files (:defaults
                   "keywords"
                   "company-maxima.el"
                   "poly-maxima.el"))
  :pin "1334f44725bd80a265de858d652f3fde4ae401fa")
;; Maxima:1 ends here

;; [[file:config.org::*IMaxima][IMaxima:1]]
;; Use the `imaxima' package bundled with the official Maxima distribution.
(package! imaxima
  :recipe (:host nil ;; Unsupported host, we will specify the complete repo link
           :repo "https://git.code.sf.net/p/maxima/code"
           :files ("interfaces/emacs/imaxima/*"))
  :pin "519ea34095e749634d3a188733a3ad284b593e12")
;; IMaxima:1 ends here

;; [[file:config.org::*Vimrc][Vimrc:1]]
(package! vimrc-mode
  :recipe (:host github
           :repo "mcandre/vimrc-mode")
  :pin "13bc150a870d5d4a95f1111e4740e2b22813c30e")
;; Vimrc:1 ends here

;; [[file:config.org::*ESS][ESS:1]]
(package! ess-view :pin "925cafd876e2cc37bc756bb7fcf3f34534b457e2")
;; ESS:1 ends here

;; [[file:config.org::*Python IDE][Python IDE:1]]
(package! elpy :pin "de31d30003c515c25ff7bfd3a361c70c298f78bb")
;; Python IDE:1 ends here

;; [[file:config.org::*Semgrep][Semgrep:1]]
(package! semgrep
  :disable t
  :recipe (:host github
           :repo "Ruin0x11/semgrep.el")
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")
;; Semgrep:1 ends here

;; [[file:config.org::*=ros.el=][=ros.el=:1]]
;; `ros.el' depends on `with-shell-interpreter' among other packages
;; See: https://github.com/DerBeutlin/ros.el/blob/master/Cask
(package! with-shell-interpreter :pin "3fd1ea892e44f7fe6f86df2b5c0a0a1e0f3913fa")
(package! ros
  :recipe (:host github
           :repo "DerBeutlin/ros.el")
  :pin "f66d2177b00b277a36c058549c477d854148623c")
;; =ros.el=:1 ends here

;; [[file:config.org::*Embed.el][Embed.el:1]]
(package! embed
  :recipe (:host github
           :repo "sjsch/embed-el")
  :pin "8df65777450c6c70a418d1bd2ba87ad590377b47")
;; Embed.el:1 ends here

;; [[file:config.org::*Arduino][Arduino:1]]
(package! arduino-mode
  :recipe (:host github
           :repo "bookest/arduino-mode")
  :pin "3e2bad4569ad26e929e6db2cbcff0d6d36812698")
;; Arduino:1 ends here

;; [[file:config.org::*Bitbake (Yocto)][Bitbake (Yocto):1]]
(package! bitbake-modes
  :recipe (:host nil
           :repo "https://bitbucket.org/olanilsson/bitbake-modes")
    :pin "a042118fd2010ef203a11e1de14e7537f8184a78")
;; Bitbake (Yocto):1 ends here

;; [[file:config.org::*WIP Company for commit messages][WIP Company for commit messages:1]]
(package! company-conventional-commits
  :recipe `(:local-repo ,(expand-file-name "lisp/company-conventional-commits" doom-user-dir)))
;; WIP Company for commit messages:1 ends here

;; [[file:config.org::*Pretty graph][Pretty graph:1]]
(package! magit-pretty-graph
  :recipe (:host github
           :repo "georgek/magit-pretty-graph")
  :pin "26dc5535a20efe781b172bac73f14a5ebe13efa9")
;; Pretty graph:1 ends here

;; [[file:config.org::*Repo][Repo:1]]
(package! repo :pin "e504aa831bfa38ddadce293face28b3c9d9ff9b7")
;; Repo:1 ends here

;; [[file:config.org::*Blamer][Blamer:1]]
(package! blamer
  :recipe (:host github
           :repo "artawower/blamer.el")
  :pin "99b43779341af0d924bfe2a9103993a6b9e3d3b2")
;; Blamer:1 ends here

;; [[file:config.org::*Assembly][Assembly:1]]
(package! nasm-mode :pin "65ca6546fc395711fac5b3b4299e76c2303d43a8")
(package! haxor-mode :pin "6fa25a8e6b6a59481bc0354c2fe1e0ed53cbdc91")
(package! mips-mode :pin "98795cdc81979821ac35d9f94ce354cd99780c67")
(package! riscv-mode :pin "8e335b9c93de93ed8dd063d702b0f5ad48eef6d7")
(package! x86-lookup :pin "1573d61cc4457737b94624598a891c837fb52c16")
;; Assembly:1 ends here

;; [[file:config.org::*Disaster][Disaster:1]]
(package! disaster :pin "0c13bd244cc43773af81e52ce73a55f199d58a61")
;; Disaster:1 ends here

;; [[file:config.org::*Devdocs][Devdocs:1]]
(package! devdocs
  :recipe (:host github
           :repo "astoff/devdocs.el"
           :files ("*.el"))
  :pin "61ce83b79dc64e2f99d7f016a09b97e14b331459")
;; Devdocs:1 ends here

;; [[file:config.org::*Systemd][Systemd:1]]
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! journalctl-mode :pin "c5bca1a5f42d2fe2a00fdf52fe480137ace971d3")
;; Systemd:1 ends here

;; [[file:config.org::*PKGBUILD][PKGBUILD:1]]
(package! pkgbuild-mode :pin "9525be8ecbd3a0d0bc7cc27e6d0f403e111aa067")
;; PKGBUILD:1 ends here

;; [[file:config.org::*Franca IDL][Franca IDL:1]]
(package! franca-idl
  :recipe (:host github
           :repo "zeph1e/franca-idl.el")
  :pin "12703ee42533bd851a1d911609020f71eb31204a")
;; Franca IDL:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
(package! aas
  :recipe (:host github
           :repo "ymarco/auto-activating-snippets")
  :pin "566944e3b336c29d3ac11cd739a954c9d112f3fb")
;; LaTeX:1 ends here

;; [[file:config.org::*Flycheck + Projectile][Flycheck + Projectile:1]]
(package! flycheck-projectile
  :recipe (:host github
           :repo "nbfalcon/flycheck-projectile")
  :pin "ce6e9e8793a55dace13d5fa13badab2dca3b5ddb")
;; Flycheck + Projectile:1 ends here

;; [[file:config.org::*Graphviz][Graphviz:1]]
(package! graphviz-dot-mode :pin "6e96a89762760935a7dff6b18393396f6498f976")
;; Graphviz:1 ends here

;; [[file:config.org::*Modula-II][Modula-II:1]]
(package! gm2-mode
  :recipe `(:local-repo ,(expand-file-name "lisp/gm2-mode" doom-user-dir)))
;; Modula-II:1 ends here

;; [[file:config.org::*Mermaid][Mermaid:1]]
(package! mermaid-mode :pin "a98a9e733b1da1e6a19e68c1db4367bf46283479")

(package! ob-mermaid
  :recipe (:host github
           :repo "arnm/ob-mermaid")
  :pin "b4ce25699e3ebff054f523375d1cf5a17bd0dbaf")
;; Mermaid:1 ends here

;; [[file:config.org::*The V Programming Language][The V Programming Language:1]]
(package! v-mode :pin "a701f4cedfff91cf4bcd17c9a2cd16a49f942743")
;; The V Programming Language:1 ends here

;; [[file:config.org::*Inspector][Inspector:1]]
(package! inspector
  :recipe (:host github
           :repo "mmontone/emacs-inspector")
  :pin "f30b735fca1c3979e693c4c76cac85885c07d8ab")
;; Inspector:1 ends here

;; [[file:config.org::*Org additional packages][Org additional packages:1]]
(unpin! org-roam) ;; To avoid problems with org-roam-ui
(package! websocket :pin "82b370602fa0158670b1c6c769f223159affce9b")
(package! org-roam-ui :pin "16a8da9e5107833032893bc4c0680b368ac423ac")
(package! org-wild-notifier :pin "9392b06d20b2f88e45a41bea17bb2f10f24fd19c")
(package! org-fragtog :pin "c675563af3f9ab5558cfd5ea460e2a07477b0cfd")
(package! org-appear :pin "60ba267c5da336e75e603f8c7ab3f44e6f4e4dac")
(package! org-super-agenda :pin "f4f528985397c833c870967884b013cf91a1da4a")
(package! doct :pin "506c22f365b75f5423810c4933856802554df464")

(package! citar-org-roam
  :recipe (:host github
           :repo "emacs-citar/citar-org-roam")
  :pin "29688b89ac3bf78405fa0dce7e17965aa8fe0dff")

(package! org-menu
  :recipe (:host github
           :repo "sheijk/org-menu")
  :pin "9cd10161c2b50dfef581f3d0441683eeeae6be59")

(package! caldav
  :recipe (:host github
           :repo "dengste/org-caldav")
  :pin "8569941a0a5a9393ba51afc8923fd7b77b73fa7a")

(package! org-ol-tree
  :recipe (:host github
           :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(package! org-modern
  :recipe (:host github
           :repo "minad/org-modern")
  :pin "828cf100c62fc9dfb50152c192ac3a968c1b54bc")

(package! org-bib
  :recipe (:host github
           :repo "rougier/org-bib-mode")
  :pin "fed9910186e5e579c2391fb356f55ae24093b55a")

(package! academic-phrases
  :recipe (:host github
           :repo "nashamri/academic-phrases")
  :pin "25d9cf67feac6359cb213f061735e2679c84187f")

(package! phscroll
  :recipe (:host github
           :repo "misohena/phscroll")
  :pin "65e00c89f078997e1a5665d069ad8b1e3b851d49")
;; Org additional packages:1 ends here
