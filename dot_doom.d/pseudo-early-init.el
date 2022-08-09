;; [[file:config.org::*Pseudo early-init][Pseudo early-init:1]]
;;; pseudo-early-init.el -*- coding: utf-8-unix; lexical-binding: t; -*-
;; Pseudo early-init:1 ends here

;; [[file:config.org::*Fixes][Fixes:1]]
;; Fixes to apply early
;; Fixes:1 ends here

;; [[file:config.org::*Useful functions][Useful functions:1]]
;; (+bool "someval")
;; --> t
(defun +bool (val) (not (null val)))

;; (+foldr (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5))
;; --> 15
;; (5 + 0) -> (4 + 5) -> (3 + 9) -> (2 + 12) --> (1 + 14)
(defun +foldr (fun acc seq)
  (if (null seq) acc
    (funcall fun (car seq) (+foldr fun acc (cdr seq)))))

;; (+foldl (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5))
;; --> 15
;; (0 + 1) -> (1 + 2) -> (3 + 3) -> (6 + 4) -> (10 + 5)
(defun +foldl (fun acc seq)
  (if (null seq) acc
    (+foldl fun (funcall fun acc (car seq)) (cdr seq))))

;; (+all '(83 88 t "txt"))
;; --> t
(defun +all (seq)
  (+foldr (lambda (r l) (and r l)) t seq))

;; (+some '(nil nil "text" nil 2))
;; --> t
(defun +some (seq)
  (+bool (+foldr (lambda (r l) (or r l)) nil seq)))

;; (+filter 'stringp '("A" 2 "C" nil 3))
;; --> ("A" "C")
(defun +filter (fun seq)
  (if (null seq) nil
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (funcall fun head)
          (cons head (+filter fun tail))
        (+filter fun tail)))))

;; (+str-join ", " '("foo" "10" "bar"))
;; --> "foo, 10, bar"
(defun +str-join (sep seq)
  (+foldl (lambda (l r) (concat l sep r))
          (car seq) (cdr seq)))

;; (+str-split "foo, 10, bar" ", ")
;; --> ("foo" "10" "bar")
(defun +str-split (str sep)
  (let ((s (string-search sep str)))
    (if s (cons (substring str 0 s)
                (+str-split (substring str (+ s (length sep))) sep))
      (list str))))

;; (+zip '(1 2 3 4) '(a b c d) '("A" "B" "C" "D"))
;; --> ((1 a "A") (2 b "B") (3 c "C") (4 d "D"))
(defun +zip (&rest seqs)
  (if (null (car seqs)) nil
    (cons (mapcar #'car seqs)
          (apply #'+zip (mapcar #'cdr seqs)))))
;; Useful functions:1 ends here

;; [[file:config.org::*Check for external tools][Check for external tools:1]]
;; Some packages do not work correctly on Emacs built with the LUCID feature
(defconst IS-LUCID (+bool (string-search "LUCID" system-configuration-features)))
(defconst EAF-DIR (expand-file-name "eaf/eaf-repo" doom-etc-dir))

(defconst AG-P (+bool (executable-find "ag")))
(defconst EAF-P (+bool (and (not IS-LUCID) (file-directory-p EAF-DIR))))
(defconst MPD-P (+all (mapcar #'executable-find '("mpc" "mpd"))))
(defconst MPV-P (+bool (executable-find "mpv")))
(defconst REPO-P (+bool (executable-find "repo")))
(defconst FRICAS-P (+bool (and (executable-find "fricas") (file-directory-p "/usr/lib/fricas/emacs"))))
(defconst MAXIMA-P (+bool (executable-find "maxima")))
(defconst QUARTO-P (+bool (executable-find "quarto")))
(defconst ROSBAG-P (+bool (executable-find "rosbag")))
(defconst ZOTERO-P (+bool (executable-find "zotero")))
(defconst CHEZMOI-P (+bool (executable-find "chezmoi")))
(defconst ECRYPTFS-P (+all (mapcar #'executable-find '("ecryptfs-add-passphrase" "/sbin/mount.ecryptfs_private"))))
(defconst BITWARDEN-P (+bool (executable-find "bw")))
(defconst YOUTUBE-DL-P (+bool (+some (mapcar #'executable-find '("yt-dlp" "youtube-dl")))))
(defconst NETEXTENDER-P (+bool (and (executable-find "netExtender") (file-exists-p "~/.local/bin/netextender") (file-exists-p "~/.ssh/sslvpn.gpg"))))
(defconst CLANG-FORMAT-P (+bool (executable-find "clang-format")))
(defconst LANGUAGETOOL-P (+bool (and (executable-find "languagetool") (string-match "\\(?:MANJARO\\|ARCH\\)" operating-system-release))))
;; Check for external tools:1 ends here
