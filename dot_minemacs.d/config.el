;;; config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; Personal info
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@fedoraproject.org")

(setq-default epa-file-encrypt-to '("F808A020A3E1AC37"))

(setq minemacs-fonts ;; or Cascadia Code, Fira Code, FiraCode Nerd Font, Iosevka Fixed Curly Slab
      '(:font-family "IBM Plex Mono" ;; "Iosevka Fixed Curly Slab"
        :font-size 15
        :variable-pitch-font-family "IBM Plex Serif"
        :variable-pitch-font-size 16))

(defvar +my/lang-main          "en")
(defvar +my/lang-secondary     "fr")
(defvar +my/lang-mother-tongue "ar")

(defvar +my/biblio-libraries-list (list (expand-file-name "~/Zotero/library.bib")))
(defvar +my/biblio-storage-list   (list (expand-file-name "~/Zotero/storage/")))
(defvar +my/biblio-notes-path     (expand-file-name "~/PhD/bibliography/notes/"))
(defvar +my/biblio-styles-path    (expand-file-name "~/Zotero/styles/"))

(setq org-directory "~/Dropbox/Org"
      source-directory (expand-file-name "~/Softwares/aur/emacs-git/src/emacs-git/")
      browse-url-chrome-program "brave")

;; Enable horizontal scrolling with the second mouse wheel or the touchpad
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-progressive-speed nil)

(with-eval-after-load 'writeroom-mode
  (with-eval-after-load 'org
    ;; Increase latex previews scale in Zen mode
    (add-hook 'writeroom-mode-enable-hook (lambda () (+org-extras-format-latex-set-scale 2.0)))
    (add-hook 'writeroom-mode-disable-hook (lambda () (+org-extras-format-latex-set-scale 1.4)))))

(with-eval-after-load 'spell-fu
  (add-hook
   'spell-fu-mode-hook
   (lambda ()
     (+spell-fu-register-dictionary "en")
     (+spell-fu-register-dictionary "fr"))))

(with-eval-after-load 'elfeed
  (setq elfeed-feeds
        '("https://arxiv.org/rss/cs.RO"
          "https://interstices.info/feed"
          "https://this-week-in-rust.org/rss.xml"
          "https://planet.emacslife.com/atom.xml"
          "https://www.omgubuntu.co.uk/feed"
          "https://itsfoss.com/feed"
          "https://linuxhandbook.com/feed"
          "https://spectrum.ieee.org/rss/robotics/fulltext"
          "https://spectrum.ieee.org/rss/aerospace/fulltext"
          "https://spectrum.ieee.org/rss/computing/fulltext"
          "https://spectrum.ieee.org/rss/blog/automaton/fulltext"
          "https://developers.redhat.com/blog/feed"
          "https://lwn.net/headlines/rss")))

(with-eval-after-load 'mu4e
  (setq mail-personal-alias-file (expand-file-name "private/mail-aliases.mailrc" minemacs-config-dir))

  ;; Add a unified inbox shortcut
  (add-to-list
   'mu4e-bookmarks
   '(:name "Unified inbox" :query "maildir:/.*inbox/" :key ?i) t)

  ;; Add shortcut to view yesterday's messages
  (add-to-list
   'mu4e-bookmarks
   '(:name "Yesterday's messages" :query "date:1d..today" :key ?y) t)

  ;; Load my accounts
  (load (expand-file-name "private/mu4e-accounts.el" minemacs-config-dir) :no-error :no-msg))

(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/Org/" ; let's put files here
        org-todo-keywords
        '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
        org-todo-keyword-faces
        '(("IDEA" . (:foreground "goldenrod" :weight bold))
          ("NEXT" . (:foreground "IndianRed1" :weight bold))
          ("STRT" . (:foreground "OrangeRed" :weight bold))
          ("WAIT" . (:foreground "coral" :weight bold))
          ("KILL" . (:foreground "DarkGreen" :weight bold))
          ("PROJ" . (:foreground "LimeGreen" :weight bold))
          ("HOLD" . (:foreground "orange" :weight bold))))

  ;; stolen from https://github.com/yohan-pereira/.emacs#babel-config
  (defun +org-confirm-babel-evaluate (lang body)
    (not (string= lang "scheme"))) ;; Don't ask for scheme

  (setq org-confirm-babel-evaluate #'+org-confirm-babel-evaluate)

  (setq org-agenda-deadline-faces
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
  (+with-shutup!
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

  (add-hook 'before-save-hook 'time-stamp nil))

(with-eval-after-load 'ox-hugo
  (setq org-hugo-auto-set-lastmod t))

(with-eval-after-load 'org-roam
  (setq org-roam-directory "~/Dropbox/Org/slip-box"
        org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))

  (advice-add #'doom-modeline-buffer-file-name
              :around
              (lambda (orig-fun)
                (if (s-contains-p org-roam-directory (or buffer-file-name ""))
                    (replace-regexp-in-string
                     "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
                     "🦄 (\\1-\\2-\\3) "
                     (subst-char-in-string ?_ ?  buffer-file-name))
                  (funcall orig-fun))))

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :if-new (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %U\n\n${body}\n")
           :unnarrowed t))))

(with-eval-after-load 'oc
  (setq org-cite-csl-styles-dir +my/biblio-styles-path
        org-cite-global-bibliography +my/biblio-libraries-list))

(with-eval-after-load 'citar
  (setq citar-library-paths +my/biblio-storage-list
        citar-notes-paths (list +my/biblio-notes-path)
        citar-bibliography +my/biblio-libraries-list))

(with-eval-after-load 'ros
  (setq ros-workspaces
        (list (ros-dump-workspace
               :tramp-prefix (format "/docker:%s@%s:" "ros" "ros-machine")
               :workspace "~/ros_ws"
               :extends '("/opt/ros/noetic/"))
              (ros-dump-workspace
               :tramp-prefix (format "/docker:%s@%s:" "ros" "ros-machine")
               :workspace "~/ros2_ws"
               :extends '("/opt/ros/foxy/"))
              (ros-dump-workspace
               :tramp-prefix (format "/ssh:%s@%s:" "swd_sk" "172.16.96.42")
               :workspace "~/ros_ws"
               :extends '("/opt/ros/noetic/"))
              (ros-dump-workspace
               :tramp-prefix (format "/ssh:%s@%s:" "swd_sk" "172.16.96.42")
               :workspace "~/ros2_ws"
               :extends '("/opt/ros/foxy/")))))
