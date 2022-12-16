;;; config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; Personal info
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@fedoraproject.org")

(setq-default epa-file-encrypt-to '("F808A020A3E1AC37"))

(setq minemacs-fonts ;; or Cascadia Code, Fira Code, FiraCode Nerd Font, Iosevka Fixed Curly Slab
      '(:font-family "Iosevka Fixed Curly Slab" ;; "Iosevka Fixed Curly Slab"
        :font-size 16
        :variable-pitch-font-family "IBM Plex Serif"
        :variable-pitch-font-size 16))

(defvar +biblio-libraries-path "~/Zotero/library.bib")
(defvar +biblio-storage-path "~/Zotero/storage/")
(defvar +biblio-notes-path "~/PhD/bibliography/notes/")
(defvar +biblio-styles-path "~/Zotero/styles/")

(setq org-directory "~/Dropbox/Org/"
      source-directory "~/Softwares/aur/emacs-git/src/emacs-git/"
      browse-url-chromium-program (or (executable-find "brave")
                                      (executable-find "chromium")
                                      (executable-find "chromium-browser"))
      browse-url-chrome-program browse-url-chromium-program)

(+eval-when-idle!
 (setq +project-scan-dir-paths '("~/PhD/workspace/"
                                 "~/PhD/workspace-no/"
                                 "~/Projects/foss/packages/"
                                 "~/Projects/foss/repos/"))
 (+shutup!
  ;; Load projects
  (+project-scan-for-projects)))

(with-eval-after-load 'projectile
  (require 'cl-lib)

  (defun +projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `+projectile-ignored-roots'"
    (cl-some
     (lambda (root)
       (file-in-directory-p
        (expand-file-name filepath)
        (expand-file-name root)))
     +projectile-ignored-roots))

  (defvar +projectile-ignored-roots
    '("~/.cache"
      "~/.emacs.d/local/"))

  (setq projectile-project-search-path
        '("~/PhD/papers"
          "~/PhD/workspace"
          "~/PhD/workspace-no"
          "~/PhD/workspace-no/ez-wheel/swd-starter-kit-repo"
          ("~/Projects/foss" . 2)) ;; ("dir" . depth)
        projectile-ignored-projects
        '("/tmp"
          "~/"
          "~/.cache"
          "~/.emacs.d/local/")
        projectile-ignored-project-function #'+projectile-ignored-project-function)

  (+eval-when-idle!
   (+shutup!
    ;; Load projects
    (projectile-discover-projects-in-search-path))))

(with-eval-after-load 'spell-fu
  (+spell-fu-register-dictionaries "en" "fr"))

(with-eval-after-load 'elfeed
  (setq elfeed-feeds
        '("https://itsfoss.com/feed"
          "https://arxiv.org/rss/cs.RO"
          "https://interstices.info/feed"
          "https://lwn.net/headlines/rss"
          "https://linuxhandbook.com/feed"
          "https://www.omgubuntu.co.uk/feed"
          "https://this-week-in-rust.org/rss.xml"
          "https://planet.emacslife.com/atom.xml"
          "https://www.technologyreview.com/feed"
          "https://developers.redhat.com/blog/feed"
          "https://spectrum.ieee.org/rss/robotics/fulltext"
          "https://spectrum.ieee.org/rss/aerospace/fulltext"
          "https://spectrum.ieee.org/rss/computing/fulltext"
          "https://spectrum.ieee.org/rss/blog/automaton/fulltext")))

(with-eval-after-load 'mu4e
  (setq mail-personal-alias-file (concat minemacs-config-dir "private/mail-aliases.mailrc"))

  ;; Add a unified inbox shortcut
  (add-to-list
   'mu4e-bookmarks
   '(:name "Unified inbox" :query "maildir:/.*inbox/" :key ?i) t)

  ;; Add shortcut to view yesterday's messages
  (add-to-list
   'mu4e-bookmarks
   '(:name "Yesterday's messages" :query "date:1d..today" :key ?y) t)

  ;; Load my accounts
  (load (concat minemacs-config-dir "private/mu4e-accounts.el") :no-error :no-msg))

(with-eval-after-load 'org
  (setq
   ;; Let's put our Org files here
   org-directory "~/Dropbox/Org/"
   ;; Default file for notes (for org-capture)
   org-default-notes-file (concat org-directory "inbox.org")
   +org-inbox-file (concat org-directory "inbox.org")
   +org-projects-file (concat org-directory "projects.org")
   ;; Custom todo keywords
   org-todo-keywords
   '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
     (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
     (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
   ;; Custom todo keyword faces
   org-todo-keyword-faces
   '(("IDEA" . (:foreground "goldenrod" :weight bold))
     ("NEXT" . (:foreground "IndianRed1" :weight bold))
     ("STRT" . (:foreground "OrangeRed" :weight bold))
     ("WAIT" . (:foreground "coral" :weight bold))
     ("KILL" . (:foreground "DarkGreen" :weight bold))
     ("PROJ" . (:foreground "LimeGreen" :weight bold))
     ("HOLD" . (:foreground "orange" :weight bold)))
   ;; Custom org-capture templates, see: https://orgmode.org/manual/Capture.html
   org-capture-templates
   `(("t" "Todo" entry (file+headline ,+org-inbox-file "Inbox")
      "* TODO %?\n%i\n%a")
     ("i" "Idea" entry (file+headline ,+org-inbox-file "Ideas")
      "* IDEA %?\n%T\n%i\n%a")
     ("p" "Project note" entry (file ,+org-projects-file)
      "* %?\n%T\n%i\n%a")
     ("n" "Note" entry (file+headline ,+org-inbox-file "Notes")
      "* NOTE %?\n%T\n%i\n%a")))

  ;; stolen from https://github.com/yohan-pereira/.emacs#babel-config
  (defun +org-confirm-babel-evaluate (lang body)
    (not (string= lang "scheme"))) ;; Don't ask for scheme

  (setq org-confirm-babel-evaluate #'+org-confirm-babel-evaluate)

  (setq org-agenda-files (list +org-inbox-file
                               +org-projects-file
                               (concat org-directory "gcal-agenda.org"))
        org-agenda-deadline-faces
        '((1.001 . error)
          (1.000 . org-warning)
          (0.500 . org-upcoming-deadline)
          (0.000 . org-upcoming-distant-deadline))
        org-list-demote-modify-bullet
        '(("+"  . "-")
          ("-"  . "+")
          ("*"  . "+")
          ("1." . "a.")))

  (setq org-export-headline-levels 5)

  ;; Needs to make a src_latex{\textsc{text}}?, with this hack you can write [[latex:textsc][Some text]].
  (+shutup!
   (org-add-link-type
    "latex" nil
    (lambda (path desc format)
      (cond
       ((eq format 'html)
        (format "<span class=\"%s\">%s</span>" path desc))
       ((eq format 'latex)
        (format "\\%s{%s}" path desc))))))

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local time-stamp-active t
                 time-stamp-start  "#\\+lastmod:[ \t]*"
                 time-stamp-end    "$"
                 time-stamp-format "%04Y-%02m-%02d")))

  (add-hook 'before-save-hook 'time-stamp))

(with-eval-after-load 'ox-hugo
  (setq org-hugo-auto-set-lastmod t))

(with-eval-after-load 'org-roam
  (setq org-roam-directory "~/Dropbox/Org/slip-box/"
        org-roam-db-location (concat org-roam-directory "org-roam.db"))

  (advice-add
   #'doom-modeline-buffer-file-name
   :around
   (defun +doom-modeline--org-roam-buffer-file-name-a (orig-fun)
     (if (string-search org-roam-directory (or buffer-file-name ""))
         (replace-regexp-in-string
          "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
          "🦄 (\\1-\\2-\\3) "
          (subst-char-in-string ?_ ?  buffer-file-name))
       (funcall orig-fun))))

  ;; Register capture template (via Org-Protocol)
  ;; Add this as bookmarklet in your browser
  ;; javascript:location.href='org-protocol://roam-ref?template=r&ref=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :if-new (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %U\n\n${body}\n")
           :unnarrowed t))))

(with-eval-after-load 'oc
  (setq org-cite-csl-styles-dir +biblio-styles-path
        org-cite-global-bibliography (ensure-list +biblio-libraries-path)))

(with-eval-after-load 'citar
  (setq citar-library-paths (ensure-list +biblio-storage-path)
        citar-notes-paths (ensure-list +biblio-notes-path)
        citar-bibliography (ensure-list +biblio-libraries-path)))

(with-eval-after-load 'empv
  (setq
   ;; Set the radio channels, you can get streams from https://www.radio-browser.info
   empv-radio-channels
   '(("El-Bahdja FM" . "http://webradio.tda.dz:8001/ElBahdja_64K.mp3")
     ("El-Chaabia" . "https://radio-dzair.net/proxy/chaabia?mp=/stream")
     ("Quran Radio" . "http://stream.radiojar.com/0tpy1h0kxtzuv")
     ("Algeria International" . "https://webradio.tda.dz/Internationale_64K.mp3")
     ("JOW Radio" . "https://str0.creacast.com/jowradio")
     ("Europe1" . "http://ais-live.cloud-services.paris:8000/europe1.mp3")
     ("France Iter" . "http://direct.franceinter.fr/live/franceinter-hifi.aac")
     ("France Info" . "http://direct.franceinfo.fr/live/franceinfo-hifi.aac")
     ("France Culture" . "http://icecast.radiofrance.fr/franceculture-hifi.aac")
     ("France Musique" . "http://icecast.radiofrance.fr/francemusique-hifi.aac")
     ("FIP" . "http://icecast.radiofrance.fr/fip-hifi.aac")
     ("Beur FM" . "http://broadcast.infomaniak.ch/beurfm-high.aac")
     ("Skyrock" . "http://icecast.skyrock.net/s/natio_mp3_128k"))
   ;; See https://docs.invidious.io/instances/
   empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1"))

(with-eval-after-load 'ros
  (setq ros-workspaces
        (list
         (ros-dump-workspace
          :tramp-prefix "/docker:ros@ros-machine:"
          :workspace "~/ros_ws"
          :extends '("/opt/ros/noetic/"))
         (ros-dump-workspace
          :tramp-prefix "/docker:ros@ros-machine:"
          :workspace "~/ros2_ws"
          :extends '("/opt/ros/foxy/"))
         (ros-dump-workspace
          :tramp-prefix "/ssh:swd_sk@172.16.96.42:"
          :workspace "~/ros_ws"
          :extends '("/opt/ros/noetic/"))
         (ros-dump-workspace
          :tramp-prefix "/ssh:swd_sk@172.16.96.42:"
          :workspace "~/ros2_ws"
          :extends '("/opt/ros/foxy/")))))

(defun +helper--in-buffer-replace (old new)
  "Replace OLD with NEW in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (matches 0))
      (while (re-search-forward old nil t)
        (replace-match new)
        (setq matches (1+ matches)))
      matches)))

(defun +helper-clear-frenchy-ponctuations ()
  "Replace french ponctuations (like unsectable space) by regular ones."
  (interactive)
  (let ((chars
         '(("[\u00a0\u200b]" . "") ;; Non-breaking and zero-width spaces
           ;; Special spaces and quads
           ("[\u2000-\u200A\u202F\u205F\u3000]" . " ")
           ("[‘’‚’]" . "'")
           ("[“”„”]" . "\"")))
        (matches 0))
    (dolist (pair chars)
      (setq matches (+ matches (+helper--in-buffer-replace (car pair) (cdr pair)))))
    (message "Replaced %d match%s." matches (if (> matches 1) "es" ""))))
