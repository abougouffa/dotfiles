;; -*- no-byte-compile: t; -*-
;;; checkers/grammar/packages.el

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-ltex :pin "ab485b8dca64922c024cb1a7ee95231d68883bca")
  ;; Optional dependency of lsp-ltex,
  ;; needed for installing/updating ltex-ls LSP server
  (package! github-tags
      :recipe (:host github
               :repo "jcs-elpa/github-tags")
      :pin "4253db4b6280640202e1d5db25239b165baa0467"))
(when (and (modulep! +lsp)
           (modulep! :tools lsp +eglot))
  (package! eglot-ltex
    :recipe (:host github
             :repo "emacs-languagetool/eglot-ltex")
    :pin "dbf45608aaa9bb61d540ce64f8b081cfa9876dd4"))
(unless (modulep! +lsp)
  (package! langtool :pin "8276eccc5587bc12fd205ee58a7a982f0a136e41"))
(package! writegood-mode :pin "d54eadeedb8bf3aa0e0a584c0a7373c69644f4b8")
