;;; modules.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; NOTE: This file is generated from "config-literate.org".

;; This file can be used to override `minemacs-modules'
;; and `minemacs-core-modules'

(setq
 ;; MinEmacs core modules
 minemacs-core-modules
 '(me-splash        ; Simple splash screen (inspired by emacs-splash)
   me-keybindings   ; general.el, which-key, hydra, ...
   me-evil          ; evil, evil-collection, evil-mc, ...
   me-core-ui       ; Theme and modeline
   me-completion)   ; vertico, marginalia, corfu, cape, consult, ...
 ;; MinEmacs modules
 minemacs-modules
 '(me-ui            ; focus, writeroom-mode, emojify, ...
   me-editor        ; yasnippet, unicode-fonts, ligature, ...
   me-extra         ; better-jumper, ...
   me-undo          ; undo-fu-session, vundo, ...
   me-multi-cursors ; iedit, evil-mc, ...
   me-vc            ; magit, forge, core-review, diff-hl, ...
   me-project       ; project, consult-project-extra, ...
   me-prog          ; tree-sitter, eglot, editorconfig, ...
   me-checkers      ; flymake, flymake-easy, gdb-mi, ...
   me-lsp           ; lsp-mode, dap-mode, consult-lsp, ...
   me-debug         ; realgud, disaster, ...
   me-emacs-lisp    ; parinfer-rust, macrostep, elisp, ...
   me-data          ; csv, yaml, toml, json, ...
   me-org           ; org, org-modern, ...
   me-notes         ; denote, consult-notes, ...
   me-email         ; mu4e, mu4e-alert, org-msg, ...
   me-lifestyle     ; awqat, ...
   me-docs          ; pdf-tools, nov, ...
   me-calendar      ; calfw, calfw-org, calfw-ical, ...
   me-latex         ; tex, auctex, reftex, ...
   me-natural-langs ; spell-fu, eglot-ltex, ...
   me-files         ; dirvish, dired, vlf, ...
   ;; me-formal     ; proof-general, alloy-mode, ...
   me-tools         ; vterm, tldr, docker, systemd, ...
   me-biblio        ; org-cite, citar, ...
   me-daemon        ; Emacs daemon tweaks
   me-tty           ; Emacs from terminal
   me-rss           ; elfeed, ...
   me-robot         ; Robotics stuff (ros, robot-mode, ...)
   me-embedded      ; Embedded systems (arduino, openocd, bitbake, ...)
   me-math          ; maxima, ess, ...
   me-modeling      ; OpenSCAD, ...
   me-workspaces    ; tabspaces, tab-bar, ...
   me-window        ; frame & window tweaks, ...
   me-media         ; empv, ...
   me-fun           ; xkcd, speed-type, ...
   me-binary)       ; hexl, decompile (using objdump)...
 ;; MinEmacs disabled packages
 minemacs-disabled-packages
 (append
  '(dashboard ein)))
