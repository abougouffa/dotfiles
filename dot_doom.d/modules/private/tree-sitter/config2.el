;;; private/tree-sitter/config2.el -*- lexical-binding: t; -*-

(eval-when! (featurep! +tree-sitter)
  ;; cc
  (add-hook! '(c-mode-local-vars-hook
               c++-mode-local-vars-hook)
             #'tree-sitter!)

  ;; json
  (add-hook! '(json-mode-local-vars-hook
               jsonc-mode-local-vars-hook)
             #'tree-sitter!)

  ;; julia
  (add-hook! 'julia-mode-local-vars-hook #'tree-sitter!)

  ;; python
  (add-hook! 'python-mode-local-vars-hook #'tree-sitter!)

  ;; sh
  (add-hook! 'sh-mode-local-vars-hook #'tree-sitter!)

  ;; web
  (add-hook! '(html-mode-local-vars-hook
               mhtml-mode-local-vars-hook
               css-mode-local-vars-hook)
             #'tree-sitter!))
