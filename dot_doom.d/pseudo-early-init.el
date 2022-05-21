;; [[file:config.org::*Pseudo early-init][Pseudo early-init:1]]
;;; pseudo-early-init.el -*- lexical-binding: t; -*-

;; Fix for #2386 until further investigation
;; From https://git.sr.ht/~gagbo/doom-config
(when noninteractive
  (after! undo-tree
    (global-undo-tree-mode -1)))
;; Pseudo early-init:1 ends here
