;; -*- no-byte-compile: t; -*-
;;; checkers/grammar/packages.el

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-ltex)
  ;; Optional dependency of lsp-ltex, needed for installing/updating ltex-ls LSP server
  (package! github-tags
    :recipe (:host github
             :repo "jcs-elpa/github-tags")
    :pin "d3f83846ec8911040b716a241b79bb32808c932d"))
(when (and (modulep! +lsp)
           (modulep! :tools lsp +eglot))
  (package! eglot-ltex
    :recipe (:host github
             :repo "emacs-languagetool/eglot-ltex")))
(unless (modulep! +lsp)
  (package! langtool :pin "8276eccc5587bc12fd205ee58a7a982f0a136e41"))
(package! writegood-mode :pin "d54eadeedb8bf3aa0e0a584c0a7373c69644f4b8")
