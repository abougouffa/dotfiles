;;; modules.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; NOTE: This file is generated from "config-literate.org".

;; This file can be used to override `minemacs-modules'
;; and `minemacs-core-modules'

(setq
;;; MinEmacs core modules
 minemacs-core-modules
 '(me-keybindings   ; general.el, which-key, hydra, ...
   me-evil          ; evil, evil-collection, evil-mc, ...
   me-core-ui       ; Theme and modeline
   me-completion)   ; vertico, marginalia, corfu, cape, consult, ...

;;; MinEmacs modules
 minemacs-modules
 '(me-ai
   me-biblio
   me-calendar
   me-checkers
   me-clojure
   me-common-lisp
   me-daemon
   me-data
   me-debug
   me-docs
   me-editor
   me-emacs-lisp
   me-email
   me-embedded
   me-extra
   me-files
   me-formal
   me-fun
   me-gtd
   me-latex
   me-lifestyle
   me-math
   me-media
   me-modeling
   me-multi-cursors
   ;; me-nano
   me-natural-langs
   me-notes
   me-org
   me-prog
   me-project
   me-robot
   me-rss
   me-scheme
   me-tags
   me-tools
   me-tty
   me-ui
   me-undo
   me-vc
   me-window
   me-workspaces)

;;; MinEmacs disabled packages
 minemacs-disabled-packages
  '(ein super-save))
