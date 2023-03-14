;;; early-config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; MinEmacs specific stuff
(unless minemacs-verbose
  (setq minemacs-msg-level 2)) ; print info messages

;; Force loading lazy packages immediately, not in idle time
;; (setq minemacs-not-lazy t)

;; Setup a `debug-on-message' to catch a wired message!
;; (setq debug-on-message "\\(?:error in process filter: \\(?:\\(?:mu4e-warn: \\)?\\[mu4e] No message at point\\)\\)")
;; (setq debug-on-message "Package cl is deprecated")

;; (setenv "MINEMACS_IGNORE_VERSION_CHECK" "1")
