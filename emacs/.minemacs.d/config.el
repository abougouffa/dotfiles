;;; config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; NOTE: This file is generated from "config-literate.org".

;; [[file:../../literate-config.org::*User information][User information:1]]
;; Personal info
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address (rot13 "nobhtbhssn@srqbencebwrpg.bet"))
;; User information:1 ends here

;; [[file:../../literate-config.org::*Crypto stuff][Crypto stuff:1]]
(setq-default
 ;; Encrypt files to my self by default
 epa-file-encrypt-to '("F808A020A3E1AC37"))
;; Crypto stuff:1 ends here

;; [[file:../../literate-config.org::*Bidirectional settings][Bidirectional settings:1]]
(setq-default
 ;; Better support for files with long lines
 bidi-paragraph-direction 'left-to-right
 ;; Speeds redisplay, may break paranthesis rendering for bidirectional files
 bidi-inhibit-bpa t)
;; Bidirectional settings:1 ends here

;; [[file:../../literate-config.org::*Directories][Directories:1]]
(defvar +biblio-notes-path (expand-file-name "~/Research/bibliography/notes/"))
(defvar +biblio-styles-path (expand-file-name "~/Zotero/styles/"))
(defvar +biblio-storage-path (expand-file-name "~/Zotero/storage/"))
(defvar +biblio-libraries-path (expand-file-name "~/Zotero/library.bib"))

(setq org-directory "~/Dropbox/Org/"
      source-directory "~/Softwares/aur/emacs-git/src/emacs-git/")
;; Directories:1 ends here

;; [[file:../../literate-config.org::*Misc][Misc:1]]
(setq +binary-hexl-enable t
      +binary-objdump-enable t
      browse-url-chromium-program (or (executable-find "brave")
                                      (executable-find "chromium")
                                      (executable-find "chromium-browser"))
      browse-url-chrome-program browse-url-chromium-program)
;; Misc:1 ends here

;; [[file:../../literate-config.org::*Awqat][Awqat:1]]
(+lazy-when! (featurep 'me-lifestyle)
  ;; Calendar settings (from `solar')
  (setq calendar-latitude 48.86
        calendar-longitude 2.35
        calendar-location-name "Paris, FR"
        calendar-time-display-form '(24-hours ":" minutes))

  (awqat-display-prayer-time-mode 1)
  (awqat-set-preset-french-muslims))
;; Awqat:1 ends here

;; [[file:../../literate-config.org::*Projects][Projects:1]]
(+lazy!
 (setq +project-scan-dir-paths
       '("~/Research/papers/"
         "~/Research/workspace/"
         "~/Research/workspace-no/"
         "~/Research/workspace-no/ez-wheel/swd-starter-kit-repo/"
         "~/Projects/foss/packages/"
         "~/Projects/foss/repos/"))

 (+shutup!
  (+project-scan-for-projects)))
;; Projects:1 ends here

;; [[file:../../literate-config.org::*Theme & font][Theme & font:1]]
(setq minemacs-theme 'doom-one-light) ; 'apropospriate-light
;; Theme & font:1 ends here

;; [[file:../../literate-config.org::*Writing mode][Writing mode:1]]
(with-eval-after-load 'me-writing-mode
  (setq +writing-mixed-pitch-enable nil
        +writing-text-scale 2.0))
;; Writing mode:1 ends here

;; [[file:../../literate-config.org::*LSP][LSP:1]]
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "pyls")
    :major-modes '(python-mode python-ts-mode)
    :remote? t
    :server-id 'pyls-remote)))
;; LSP:1 ends here

;; [[file:../../literate-config.org::*Spell-fu][Spell-fu:1]]
(with-eval-after-load 'spell-fu
  (+spell-fu-register-dictionaries! "en" "fr"))
;; Spell-fu:1 ends here

;; [[file:../../literate-config.org::*News feed (=elfeed=)][News feed (=elfeed=):1]]
(with-eval-after-load 'elfeed
  (setq elfeed-feeds
        '(("https://arxiv.org/rss/cs.RO" robotics academic)
          ("https://interstices.info/feed" science academic)
          ("https://spectrum.ieee.org/rss/robotics/fulltext" robotics academic)
          ("https://spectrum.ieee.org/rss/aerospace/fulltext" academic aerospace)
          ("https://spectrum.ieee.org/rss/computing/fulltext" academic computing)
          ("https://spectrum.ieee.org/rss/blog/automaton/fulltext" academic automation robotics)
          ("https://www.technologyreview.com/feed" tech science)
          ("https://itsfoss.com/feed" linux foss)
          ("https://lwn.net/headlines/rss" linux foss)
          ("https://linuxhandbook.com/feed" linux foss)
          ("https://www.omgubuntu.co.uk/feed" linux foss)
          ("https://this-week-in-rust.org/rss.xml" rust prog)
          ("https://planet.emacslife.com/atom.xml" emacs prog foss)
          ("https://developers.redhat.com/blog/feed" linux foss))))
;; News feed (=elfeed=):1 ends here

;; [[file:../../literate-config.org::*Mail client and indexer (=mu= and =mu4e=)][Mail client and indexer (=mu= and =mu4e=):1]]
;; To disable auto starting mu4e in background
(setq +mu4e-auto-start nil)

(with-eval-after-load 'mu4e
  ;; Custom files
  (setq mail-personal-alias-file (concat minemacs-config-dir "private/mail-aliases.mailrc")
        mu4e-icalendar-diary-file (concat org-directory "icalendar-diary.org"))

  ;; Add a unified inbox shortcut
  (add-to-list
   'mu4e-bookmarks
   '(:name "Unified inbox" :query "maildir:/.*inbox/" :key ?i) t)

  ;; Add shortcut to view spam messages
  (add-to-list
   'mu4e-bookmarks
   '(:name "Spams" :query "maildir:/.*\\(spam\\|junk\\).*/" :key ?s) t)

  ;; The `+mu4e-extras-ignore-spams-query' function is defined in
  ;; `me-mu4e-extras'.
  (with-eval-after-load 'me-mu4e-extras
    ;; Add shortcut to view yesterday's messages
    (add-to-list
     'mu4e-bookmarks
     `(:name "Yesterday's messages" :query ,(+mu4e-extras-ignore-spams-query "date:1d..today") :key ?y) t))

  ;; Load my accounts
  (+load minemacs-config-dir "private/mu4e-accounts.el")
  (+load minemacs-config-dir "private/mu4e-extra-commands.el"))
;; Mail client and indexer (=mu= and =mu4e=):1 ends here

;; [[file:../../literate-config.org::*EMPV][EMPV:1]]
(with-eval-after-load 'empv
  (setq
   ;; Set the radio channels, you can get streams from https://www.radio-browser.info
   empv-radio-channels
   '(("El-Bahdja FM"          . "http://webradio.tda.dz:8001/ElBahdja_64K.mp3")
     ("El-Chaabia"            . "https://radio-dzair.net/proxy/chaabia?mp=/stream")
     ("Quran Radio"           . "http://stream.radiojar.com/0tpy1h0kxtzuv")
     ("Algeria International" . "https://webradio.tda.dz/Internationale_64K.mp3")
     ("JOW Radio"             . "https://str0.creacast.com/jowradio")
     ("France Iter"           . "http://direct.franceinter.fr/live/franceinter-hifi.aac")
     ("France Info"           . "http://direct.franceinfo.fr/live/franceinfo-hifi.aac")
     ("France Culture"        . "http://icecast.radiofrance.fr/franceculture-hifi.aac")
     ("France Musique"        . "http://icecast.radiofrance.fr/francemusique-hifi.aac")
     ("FIP"                   . "http://icecast.radiofrance.fr/fip-hifi.aac")
     ("Beur FM"               . "http://broadcast.infomaniak.ch/beurfm-high.aac")
     ("Skyrock"               . "http://icecast.skyrock.net/s/natio_mp3_128k"))
   ;; See https://docs.invidious.io/instances/
   empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1"))
;; EMPV:1 ends here

;; [[file:../../literate-config.org::*Tramp][Tramp:1]]
(with-eval-after-load 'tramp
  (setq
   ;; Do not use a separate history file for tramp sessions (buggy!)
   tramp-histfile-override nil
   ;; Use Bash as a default remote shell
   tramp-default-remote-shell "/bin/bash"
   ;; Use bash for encoding and decoding commands on the local host
   tramp-encoding-shell "/bin/bash"))
;; Tramp:1 ends here

;; [[file:../../literate-config.org::*Robot Operating System (ROS)][Robot Operating System (ROS):1]]
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
;; Robot Operating System (ROS):1 ends here

;; [[file:../../literate-config.org::*Org mode tweaks][Org mode tweaks:1]]
(with-eval-after-load 'org
  (setq
   ;; Let's put our Org files here
   org-directory "~/Dropbox/Org/"
   ;; Do not ask before tangling
   org-confirm-babel-evaluate nil
   ;; The last level which is still exported as a headline
   org-export-headline-levels 5
   ;; Default file for notes (for org-capture)
   org-default-notes-file (concat org-directory "inbox.org")
   +org-inbox-file (concat org-directory "inbox.org")
   +org-projects-file (concat org-directory "projects.org")
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

  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("home"      . ?h)
          ("research"  . ?r)
          ("work"      . ?w)
          (:endgroup   . nil)
          (:startgroup . nil)
          ("tool"      . ?o)
          ("dev"       . ?d)
          ("report"    . ?p)
          (:endgroup   . nil)
          (:startgroup . nil)
          ("easy"      . ?e)
          ("medium"    . ?m)
          ("hard"      . ?a)
          (:endgroup   . nil)
          ("urgent"    . ?u)
          ("key"       . ?k)
          ("bonus"     . ?b)
          ("ignore"    . ?i)
          ("noexport"  . ?x)))

  (setq org-tag-faces
        '(("home"     . (:foreground "goldenrod"  :weight bold))
          ("research" . (:foreground "goldenrod"  :weight bold))
          ("work"     . (:foreground "goldenrod"  :weight bold))
          ("tool"     . (:foreground "IndianRed1" :weight bold))
          ("dev"      . (:foreground "IndianRed1" :weight bold))
          ("report"   . (:foreground "IndianRed1" :weight bold))
          ("urgent"   . (:foreground "red"        :weight bold))
          ("key"      . (:foreground "red"        :weight bold))
          ("easy"     . (:foreground "green4"     :weight bold))
          ("medium"   . (:foreground "orange"     :weight bold))
          ("hard"     . (:foreground "red"        :weight bold))
          ("bonus"    . (:foreground "goldenrod"  :weight bold))
          ("ignore"   . (:foreground "Gray"       :weight bold))
          ("noexport" . (:foreground "LimeGreen"  :weight bold))))

  ;; stolen from https://github.com/yohan-pereira/.emacs#babel-config
  ;; (defun +org-confirm-babel-evaluate (lang body)
  ;;   (not (string= lang "scheme"))) ;; Don't ask for scheme

  ;; (setq org-confirm-babel-evaluate #'+org-confirm-babel-evaluate)

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

  ;; Needs to make a src_latex{\textsc{text}}?, with this hack you can write [[latex:textsc][Some text]].
  (+shutup!
   (org-add-link-type
    "latex" nil
    (lambda (path desc format)
      (cond
       ((eq format 'html)
        (format "<span class=\"%s\">%s</span>" path desc))
       ((eq format 'latex)
        (format "{\\%s{%s}}" path desc))))))

  (add-hook
   'org-mode-hook
   (defun +org--time-stamp-setup-h ()
     (setq-local
      time-stamp-active t
      time-stamp-format "%04Y-%02m-%02d"
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$")))

  (add-hook 'before-save-hook #'time-stamp))
;; Org mode tweaks:1 ends here

;; [[file:../../literate-config.org::*Org + Hugo][Org + Hugo:1]]
(with-eval-after-load 'ox-hugo
  (setq org-hugo-auto-set-lastmod t))
;; Org + Hugo:1 ends here

;; [[file:../../literate-config.org::*Org + LaTeX][Org + LaTeX:1]]
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-packages-alist '("svgnames" "xcolor")))
;; Org + LaTeX:1 ends here

;; [[file:../../literate-config.org::*Denote][Denote:1]]
(setq denote-directory "~/Dropbox/Org/notes/")

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude denote-directory))
;; Denote:1 ends here

;; [[file:../../literate-config.org::*Org-cite][Org-cite:1]]
(with-eval-after-load 'oc
  (setq org-cite-csl-styles-dir +biblio-styles-path
        org-cite-global-bibliography (ensure-list +biblio-libraries-path)))
;; Org-cite:1 ends here

;; [[file:../../literate-config.org::*Citar][Citar:1]]
(with-eval-after-load 'citar
  (setq citar-library-paths (ensure-list +biblio-storage-path)
        citar-notes-paths (ensure-list +biblio-notes-path)
        citar-bibliography (ensure-list +biblio-libraries-path)))
;; Citar:1 ends here

;; [[file:../../literate-config.org::*Machine specific overwrites][Machine specific overwrites:1]]
(let ((machine-specific-conf (concat minemacs-config-dir "private/machine-specific.el")))
  (when (file-exists-p machine-specific-conf)
    (+load machine-specific-conf)))
;; Machine specific overwrites:1 ends here
