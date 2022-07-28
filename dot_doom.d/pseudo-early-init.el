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
(defun bool (val) (not (null val))) ;; Convert a value to boolean

(defconst ZOTERO-OK-P (bool (executable-find "zotero")))
(defconst LANGUAGETOOL-OK-P (bool (executable-find "languagetool")))
(defconst AG-OK-P (bool (executable-find "ag")))
(defconst CHEZMOI-OK-P (bool (executable-find "chezmoi")))
(defconst BITWARDEN-OK-P (bool (executable-find "bw")))
(defconst REPO-OK-P (bool (executable-find "repo")))
(defconst MAXIMA-OK-P (bool (executable-find "maxima")))
(defconst QUARTO-OK-P (bool (executable-find "quarto")))
(defconst CLANG-FORMAT-OK-P (bool (executable-find "clang-format")))
(defconst ROSBAG-OK-P (bool (executable-find "rosbag")))

(defconst FRICAS-OK-P
  (bool (and (executable-find "fricas")
             (file-directory-p "/usr/lib/fricas/emacs"))))

(defconst EAF-OK-P
  (bool (and (file-directory-p (expand-file-name "emacs-application-framework" doom-etc-dir))
             ;; EAF doesn't work with LUCID build, however, I found LUCID more stable for
             ;; Emacs daemon + emacsclient usage. So, this section will not be used for LUCID builds.
             (not (string-search "LUCID" system-configuration-features)))))

(defconst NETEXTENDER-OK-P
  (let ((ok (bool (and (executable-find "netExtender")
                       (file-exists-p "~/.local/bin/netextender")
                       (file-exists-p "~/.ssh/netExtender-params.gpg")))))
    (unless ok (warn "Missing netExtender dependencies."))
    ok)
  "Evaluates to 't' when a valid netExtender configuration is present, 'nil' otherwise.")

(defconst MPD-OK-P
  (let ((ok (bool (and (executable-find "mpc") (executable-find "mpd")))))
    (unless ok (warn "Missing MPD or MPC. Falling back to the EMMS default backend."))
    ok)
  "Evaluates to 't' when MPD and MPC commands are present, 'nil' otherwise.")

(defconst MPV-OK-P
  (let ((ok (bool (and MPD-OK-P
                       (executable-find "mpv")
                       (executable-find "youtube-dl")))))
    (unless ok (warn "Missing MPV or youtube-dl."))
    (and nil ok)) ;; NOTE: disabled
  "Evaluates to 't' when MPV and youtube-dl commands are present, 'nil' otherwise.")
;; Check for external tools:1 ends here
