;; [[file:config.org::*Pseudo early-init][Pseudo early-init:1]]
;;; pseudo-early-init.el -*- coding: utf-8-unix; lexical-binding: t; -*-
;; Pseudo early-init:1 ends here

;; [[file:config.org::*Useful functions][Useful functions:1]]
;;; === Primitives ===

;; (+bool "someval") ;; ==> t
(defun +bool (val) (not (null val)))

;;; === Higher order functions ===

;; (+foldr (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (5 + 0) -> (4 + 5) -> (3 + 9) -> (2 + 12) --> (1 + 14)
(defun +foldr (fun acc seq)
  (if (null seq) acc
    (funcall fun (car seq) (+foldr fun acc (cdr seq)))))

;; (+foldl (lambda (a b) (message "(%d + %d)" a b) (+ a b)) 0 '(1 2 3 4 5)) ;; ==> 15
;; (0 + 1) -> (1 + 2) -> (3 + 3) -> (6 + 4) -> (10 + 5)
(defun +foldl (fun acc seq)
  (if (null seq) acc
    (+foldl fun (funcall fun acc (car seq)) (cdr seq))))

;; (+all '(83 88 t "txt")) ;; ==> t
(defun +all (seq)
  (+foldr (lambda (r l) (and r l)) t seq))

;; (+some '(nil nil "text" nil 2)) ;; ==> t
(defun +some (seq)
  (+bool (+foldr (lambda (r l) (or r l)) nil seq)))

;; (+filter 'stringp '("A" 2 "C" nil 3)) ;; ==> ("A" "C")
(defun +filter (fun seq)
  (when seq
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (funcall fun head)
          (cons head (+filter fun tail))
        (+filter fun tail)))))

;; (+zip '(1 2 3 4) '(a b c d) '("A" "B" "C" "D")) ;; ==> ((1 a "A") (2 b "B") (3 c "C") (4 d "D"))
(defun +zip (&rest seqs)
  (if (null (car seqs)) nil
    (cons (mapcar #'car seqs)
          (apply #'+zip (mapcar #'cdr seqs)))))

;;; === Strings ===

;; (+str-join ", " '("foo" "10" "bar")) ;; ==> "foo, 10, bar"
(defun +str-join (sep seq)
  (+foldl (lambda (l r) (concat l sep r))
          (car seq) (cdr seq)))

;; (+str-split "foo, 10, bar" ", ") ;; ==> ("foo" "10" "bar")
(defun +str-split (str sep)
  (let ((s (string-search sep str)))
    (if s (cons (substring str 0 s)
                (+str-split (substring str (+ s (length sep))) sep))
      (list str))))

(defun +str-replace (old new s)
  "Replaces OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun +str-replace-all (replacements s)
  "REPLACEMENTS is a list of cons-cells. Each `car` is replaced with `cdr` in S."
  (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                            (lambda (it) (cdr (assoc-string it replacements)))
                            s t t))

;;; === Files, IO ===

(defun +file-mime-type (file)
  "Get MIME type for FILE based on magic codes provided by the 'file' command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc."
  (let ((mime-type (shell-command-to-string (format "file --brief --mime-type %s" file))))
    (intern (string-trim-right mime-type))))

(defun +file-name-incremental (filename)
  "Return an unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (file (file-name-base filename))
         (filename-regex (concat "^" file "\\(?:-\\(?1:[[:digit:]]+\\)\\)?" (if ext (concat "\\." ext) "")))
         (last-file (car (last (directory-files dir nil filename-regex))))
         (last-file-num (when (and last-file (string-match filename-regex last-file) (match-string 1 last-file))))
         (num (1+ (string-to-number (or last-file-num "-1"))))
         (filename (file-name-concat dir (format "%s%s%s" file (if last-file (format "-%d" num) "") (if ext (concat "." ext) "")))))
    filename))

(defun +file-read-to-string (filename)
  "Return a string with the contents of FILENAME."
  (when (and (file-exists-p filename) (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

;;; === Systemd ===

(defun +systemd-running-p (service)
  "Check if the systemd SERVICE is running."
  (zerop (call-process "systemctl" nil nil nil "--user" "is-active" "--quiet" service ".service")))

(defun +systemd-command (service command &optional pre-fn post-fn)
  "Call systemd with COMMAND and SERVICE."
  (interactive)
  (when pre-fn (funcall pre-fn))
  (let ((success (zerop (call-process "systemctl" nil nil nil "--user" command service ".service"))))
    (unless success
      (message "[systemd]: Failed on calling '%s' on service %s.service." command service))
    (when post-fn (funcall post-fn success))
    success))

(defun +systemd-start (service &optional pre-fn post-fn)
  "Start systemd SERVICE."
  (interactive)
  (+systemd-command service "start" pre-fn post-fn))

(defun +systemd-stop (service &optional pre-fn post-fn)
  "Stops the systemd SERVICE."
  (interactive)
  (+systemd-command service "stop" pre-fn post-fn))
;; Useful functions:1 ends here

;; [[file:config.org::*Fixes][Fixes:1]]
;; Fixes to apply early

(when (daemonp)
  ;; When starting Emacs in daemon mode,
  ;; I need to have a valid passphrase in the gpg-agent.
  (let ((try-again 3)
        unlocked)
    (while (not (or unlocked (zerop try-again)))
      (setq unlocked (zerop (shell-command "gpg -q --no-tty --logger-file /dev/null --batch -d ~/.authinfo.gpg > /dev/null" nil nil))
            try-again (1- try-again))
      (unless unlocked
        (message "GPG: failed to unlock, please try again (%d)" try-again)))
    (unless unlocked ;; Exit Emacs, systemd will restart it
      (kill-emacs 1))))
;; Fixes:1 ends here

;; [[file:config.org::*Check for external tools][Check for external tools:1]]
(defconst EAF-DIR (expand-file-name "eaf/eaf-repo" doom-data-dir))
(defconst IS-LUCID (string-search "LUCID" system-configuration-features))

(defconst AG-P (executable-find "ag"))
(defconst EAF-P (and (not IS-LUCID) (file-directory-p EAF-DIR)))
(defconst MPD-P (+all (mapcar #'executable-find '("mpc" "mpd"))))
(defconst MPV-P (executable-find "mpv"))
(defconst REPO-P (executable-find "repo"))
(defconst FRICAS-P (and (executable-find "fricas") (file-directory-p "/usr/lib/fricas/emacs")))
(defconst MAXIMA-P (executable-find "maxima"))
(defconst QUARTO-P (executable-find "quarto"))
(defconst ROSBAG-P (executable-find "rosbag"))
(defconst ZOTERO-P (executable-find "zotero"))
(defconst CHEZMOI-P (executable-find "chezmoi"))
(defconst OBJDUMP-P (executable-find "objdump"))
(defconst ECRYPTFS-P (+all (mapcar #'executable-find '("ecryptfs-add-passphrase" "/sbin/mount.ecryptfs_private"))))
(defconst BITWARDEN-P (executable-find "bw"))
(defconst YOUTUBE-DL-P (+some (mapcar #'executable-find '("yt-dlp" "youtube-dl"))))
(defconst NETEXTENDER-P (and (executable-find "netExtender") (+all (mapcar #'file-exists-p '("~/.local/bin/netextender" "~/.ssh/sslvpn.gpg")))))
(defconst CLANG-FORMAT-P (executable-find "clang-format"))
(defconst LANGUAGETOOL-P (executable-find "languagetool"))
;; Check for external tools:1 ends here
