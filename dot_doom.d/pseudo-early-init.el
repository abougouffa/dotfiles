;; [[file:config.org::*Pseudo early-init][Pseudo early-init:1]]
;;; pseudo-early-init.el -*- lexical-binding: t; -*-
;; Pseudo early-init:1 ends here

;; [[file:config.org::*Fixes][Fixes:1]]
;; Fix for #2386 until further investigation
;; From https://git.sr.ht/~gagbo/doom-config
(when noninteractive
  (after! undo-tree
    (global-undo-tree-mode -1)))
;; Fixes:1 ends here

;; [[file:config.org::*Check for external tools][Check for external tools:1]]
(defun bool (val) (not (null val)))

;; Some packages do not work correctly on Emacs built with the LUCID feature
(defconst IS-LUCID (bool (string-search "LUCID" system-configuration-features)))

(defconst AG-P (bool (executable-find "ag")))
(defconst EAF-P (bool (and (not IS-LUCID) (file-directory-p (expand-file-name "eaf/eaf-repo" doom-etc-dir)))))
(defconst MPD-P (bool (and (executable-find "mpc") (executable-find "mpd"))))
(defconst MPV-P (bool (executable-find "mpv")))
(defconst REPO-P (bool (executable-find "repo")))
(defconst FRICAS-P (bool (and (executable-find "fricas") (file-directory-p "/usr/lib/fricas/emacs"))))
(defconst MAXIMA-P (bool (executable-find "maxima")))
(defconst QUARTO-P (bool (executable-find "quarto")))
(defconst ROSBAG-P (bool (executable-find "rosbag")))
(defconst ZOTERO-P (bool (executable-find "zotero")))
(defconst CHEZMOI-P (bool (executable-find "chezmoi")))
(defconst BITWARDEN-P (bool (executable-find "bw")))
(defconst YOUTUBE-DL-P (bool (or (executable-find "yt-dlp") (executable-find "youtube-dl"))))
(defconst NETEXTENDER-P (bool (and (executable-find "netExtender") (file-exists-p "~/.local/bin/netextender") (file-exists-p "~/.ssh/sslvpn.gpg"))))
(defconst CLANG-FORMAT-P (bool (executable-find "clang-format")))
(defconst LANGUAGETOOL-P (bool (executable-find "languagetool")))
;; Check for external tools:1 ends here
