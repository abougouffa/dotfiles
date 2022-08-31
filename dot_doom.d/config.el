;;; config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; [[file:config.org::*User information][User information:1]]
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@fedoraproject.org")
;; User information:1 ends here

;; [[file:config.org::*Common variables][Common variables:1]]
(defvar +my/lang-mother-tongue "ar")
(defvar +my/lang-main          "en")
(defvar +my/lang-secondary     "fr")

(defvar +my/biblio-libraries-list (list (expand-file-name "~/Zotero/library.bib")))
(defvar +my/biblio-storage-list   (list (expand-file-name "~/Zotero/storage/")))
(defvar +my/biblio-notes-path     (expand-file-name "~/PhD/bibliography/notes/"))
(defvar +my/biblio-styles-path    (expand-file-name "~/Zotero/styles/"))

;; Set it early, to avoid creating "~/org" at startup
(setq org-directory "~/Dropbox/Org")
;; Common variables:1 ends here

;; [[file:config.org::*Secrets][Secrets:1]]
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-do-cache t
      auth-source-cache-expiry 86400 ; All day, defaut is 2h (7200)
      password-cache t
      password-cache-expiry 86400)

(after! epa
  (setq-default epa-file-encrypt-to '("F808A020A3E1AC37")))
;; Secrets:1 ends here

;; [[file:config.org::*File deletion][File deletion:1]]
(setq-default delete-by-moving-to-trash t
              trash-directory nil) ;; Use freedesktop.org trashcan
;; File deletion:1 ends here

;; [[file:config.org::*Window][Window:1]]
(setq-default window-combination-resize t)
;; Window:1 ends here

;; [[file:config.org::*Split defaults][Split defaults:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Split defaults:1 ends here

;; [[file:config.org::*Split defaults][Split defaults:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
;; Split defaults:2 ends here

;; [[file:config.org::*Messages buffer][Messages buffer:1]]
(defvar +messages--auto-tail-enabled nil)

(defun +messages--auto-tail-a (&rest arg)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name (buffer-name (messages-buffer)))
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))

(defun +messages-auto-tail-toggle ()
  "Auto tail the '*Messages*' buffer."
  (interactive)
  (if +messages--auto-tail-enabled
      (progn
        (advice-remove 'message '+messages--auto-tail-a)
        (setq +messages--auto-tail-enabled nil)
        (message "+messages-auto-tail: Disabled."))
    (advice-add 'message :after '+messages--auto-tail-a)
    (setq +messages--auto-tail-enabled t)
    (message "+messages-auto-tail: Enabled.")))
;; Messages buffer:1 ends here

;; [[file:config.org::*Auto-save][Auto-save:2]]
(use-package! super-save
  :ensure t
  :config
  (setq auto-save-default t ;; nil to switch off the built-in `auto-save-mode', maybe leave it t to have a backup!
        super-save-exclude '(".gpg")
        super-save-remote-files nil
        super-save-auto-save-when-idle t)
  (super-save-mode +1))
;; Auto-save:2 ends here

;; [[file:config.org::*Auto-save][Auto-save:3]]
(setq auto-save-default t) ;; enable built-in `auto-save-mode'
;; Auto-save:3 ends here

;; [[file:config.org::*Undo][Undo:1]]
;; Increase undo history limits even more
(after! undo-fu
  (setq undo-limit        10000000     ;; 1MB   (default is 160kB, Doom's default is 400kB)
        undo-strong-limit 100000000    ;; 100MB (default is 240kB, Doom's default is 3MB)
        undo-outer-limit  1000000000)) ;; 1GB   (default is 24MB,  Doom's default is 48MB)

(after! evil
  (setq evil-want-fine-undo t)) ;; By default while in insert all changes are one big blob
;; Undo:1 ends here

;; [[file:config.org::*Visual Undo (=vundo=)][Visual Undo (=vundo=):2]]
(use-package! vundo
  :defer t
  :init
  (defconst +vundo-unicode-symbols
   '((selected-node   . ?●)
     (node            . ?○)
     (vertical-stem   . ?│)
     (branch          . ?├)
     (last-branch     . ?╰)
     (horizontal-stem . ?─)))

  (map! :leader
        (:prefix ("o")
         :desc "vundo" "v" #'vundo))

  :config
  (setq vundo-glyph-alist +vundo-unicode-symbols
        vundo-compact-display t
        vundo-window-max-height 6))
;; Visual Undo (=vundo=):2 ends here

;; [[file:config.org::*Editing][Editing:1]]
;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

;; Enable relative line numbers
(setq display-line-numbers-type 'relative)

;; Iterate through CamelCase words
(global-subword-mode 1)
;; Editing:1 ends here

;; [[file:config.org::*Emacs sources][Emacs sources:1]]
(setq source-directory
      (expand-file-name "~/Softwares/src/emacs"))
;; Emacs sources:1 ends here

;; [[file:config.org::*Browsers][Browsers:1]]
(setq browse-url-chrome-program "brave")
;; Browsers:1 ends here

;; [[file:config.org::*Initialization][Initialization:1]]
(defun +daemon-startup ()
  ;; mu4e
  (when (require 'mu4e nil t)
    ;; Automatically start `mu4e' in background.
    (when (load! "mu-lock.el" (expand-file-name "email/mu4e/autoload" doom-modules-dir) t)
      (setq +mu4e-lock-greedy t
            +mu4e-lock-relaxed t)
      (when (+mu4e-lock-available t)
        (mu4e--start)))

    ;; Check each 5m, if `mu4e' if closed, start it in background.
    (run-at-time
     60 (* 60 5) ;; Check each 5 minutes
     (lambda ()
       (when (and (not (mu4e-running-p)) (+mu4e-lock-available))
         (mu4e--start)
         (message "Started `mu4e' in background.")))))

  ;; RSS
  (when (require 'elfeed nil t)
    (run-at-time nil (* 2 60 60) #'elfeed-update))) ;; Check every 2h

(when (daemonp)
  ;; Daemon startup
  (add-hook 'emacs-startup-hook #'+daemon-startup)
  ;; After creating a new frame (via emacsclient)
  (add-hook!
   'server-after-make-frame-hook
   ;; Reload Doom's theme
   #'doom/reload-theme
   ;; Switch to Dashboard, unless we started in one of the special buffers
   (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
     (switch-to-buffer +doom-dashboard-name))))
;; Initialization:1 ends here

;; [[file:config.org::*Save recent files][Save recent files:1]]
(when (daemonp)
  (add-hook! '(delete-frame-functions delete-terminal-functions)
    (let ((inhibit-message t))
      (recentf-save-list)
      (savehist-save))))
;; Save recent files:1 ends here

;; [[file:config.org::*Font][Font:1]]
(setq doom-font (font-spec :family "Iosevka Fixed" :size 20)
      doom-big-font (font-spec :family "Iosevka Fixed" :size 30 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Fixed")
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Iosevka Fixed" :weight 'light))
;; Font:1 ends here

;; [[file:config.org::*Doom][Doom:1]]
(setq doom-theme 'doom-one-light)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
;; Doom:1 ends here

;; [[file:config.org::*Clock][Clock:1]]
(after! doom-modeline
  (setq display-time-string-forms
        '((propertize (concat " 🕘 " 24-hours ":" minutes))))
  (display-time-mode 1)) ; Enable time in the mode-line
;; Clock:1 ends here

;; [[file:config.org::*Battery][Battery:1]]
(after! doom-modeline
  (let ((battery-str (battery)))
    (unless (or (equal "Battery status not available" battery-str)
                (string-match-p (regexp-quote "unknown") battery-str)
                (string-match-p (regexp-quote "N/A") battery-str))
      (display-battery-mode 1))))
;; Battery:1 ends here

;; [[file:config.org::*Mode line customization][Mode line customization:1]]
(after! doom-modeline
  (setq doom-modeline-bar-width 4
        doom-modeline-mu4e t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))
;; Mode line customization:1 ends here

;; [[file:config.org::*Custom splash image][Custom splash image:1]]
(setq fancy-splash-image (expand-file-name "assets/doom-emacs-gray.svg" doom-user-dir))
;; Custom splash image:1 ends here

;; [[file:config.org::*Dashboard][Dashboard:1]]
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-hook! '+doom-dashboard-mode-hook (hl-line-mode -1) (hide-mode-line-mode 1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
;; Dashboard:1 ends here

;; [[file:config.org::*Which key][Which key:1]]
(setq which-key-idle-delay 0.5 ;; Default is 1.0
      which-key-idle-secondary-delay 0.05) ;; Default is nil
;; Which key:1 ends here

;; [[file:config.org::*Which key][Which key:2]]
(setq which-key-allow-multiple-replacements t)

(after! which-key
  (pushnew! which-key-replacement-alist
            '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅔·\\1"))
            '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "Ⓔ·\\1"))))
;; Which key:2 ends here

;; [[file:config.org::*Window title][Window title:1]]
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string ".*/[0-9]*-?" "☰ "
                                       (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let* ((project-name (projectile-project-name))
                (project-name (if (string= "-" project-name)
                                  (ignore-errors (file-name-base (string-trim-right (vc-root-dir))))
                                project-name)))
           (when project-name
             (format (if (buffer-modified-p) " ○ %s" " ● %s") project-name))))))
;; Window title:1 ends here

;; [[file:config.org::*SVG tag and =svg-lib=][SVG tag and =svg-lib=:2]]
(use-package! svg-tag-mode
  :commands svg-tag-mode
  :config
  (setq svg-tag-tags
        '(("^\\*.* .* \\(:[A-Za-z0-9]+\\)" .
           ((lambda (tag)
              (svg-tag-make
               tag
               :beg 1
               :font-family "Roboto Mono"
               :font-size 10
               :height 0.8
               :padding 0
               :margin 0))))
          ("\\(:[A-Za-z0-9]+:\\)$" .
           ((lambda (tag)
              (svg-tag-make
               tag
               :beg 1
               :end -1
               :font-family "Roboto Mono"
               :font-size 10
               :height 0.8
               :padding 0
               :margin 0)))))))
;; SVG tag and =svg-lib=:2 ends here

;; [[file:config.org::*SVG tag and =svg-lib=][SVG tag and =svg-lib=:3]]
(after! svg-lib
  ;; Set `svg-lib' cache directory
  (setq svg-lib-icons-dir (expand-file-name "svg-lib" doom-data-dir)))
;; SVG tag and =svg-lib=:3 ends here

;; [[file:config.org::*Focus][Focus:2]]
(use-package! focus
  :commands focus-mode)
;; Focus:2 ends here

;; [[file:config.org::*Scrolling][Scrolling:2]]
(use-package! good-scroll
  :unless EMACS29+
  :config (good-scroll-mode 1))

(when EMACS29+
  (pixel-scroll-precision-mode 1))

(setq hscroll-step 1
      hscroll-margin 0
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil)
;; Scrolling:2 ends here

;; [[file:config.org::*All the icons][All the icons:1]]
(after! all-the-icons
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))
;; All the icons:1 ends here

;; [[file:config.org::*Tabs][Tabs:1]]
(after! centaur-tabs
  ;; For some reason, setting `centaur-tabs-set-bar' this to `right'
  ;; instead of Doom's default `left', fixes this issue with Emacs daemon:
  ;; https://github.com/doomemacs/doomemacs/issues/6647#issuecomment-1229365473
  (setq centaur-tabs-set-bar 'right
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "⨂"
        centaur-tabs-modified-marker "⨀"))
;; Tabs:1 ends here

;; [[file:config.org::*File templates][File templates:1]]
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)
;; File templates:1 ends here

;; [[file:config.org::*Scratch buffer][Scratch buffer:1]]
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode)
;; Scratch buffer:1 ends here

;; [[file:config.org::*Mouse buttons][Mouse buttons:1]]
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

;; Enable horizontal scrolling with the second mouse wheel or the touchpad
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-progressive-speed nil)
;; Mouse buttons:1 ends here

;; [[file:config.org::*Very large files][Very large files:2]]
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)
;; Very large files:2 ends here

;; [[file:config.org::*Evil][Evil:1]]
(after! evil
  ;; This fixes https://github.com/doomemacs/doomemacs/issues/6478
  ;; Ref: https://github.com/emacs-evil/evil/issues/1630
  (evil-select-search-module 'evil-search-module 'isearch)

  (setq evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring
;; Evil:1 ends here

;; [[file:config.org::*Aggressive indent][Aggressive indent:2]]
(use-package! aggressive-indent
  :commands (aggressive-indent-mode))
;; Aggressive indent:2 ends here

;; [[file:config.org::*YASnippet][YASnippet:1]]
(setq yas-triggers-in-field t)
;; YASnippet:1 ends here

;; [[file:config.org::*Company][Company:1]]
(setq company-global-modes
      '(not erc-mode
            circe-mode
            message-mode
            help-mode
            gud-mode
            vterm-mode
            org-mode))
;; Company:1 ends here

;; [[file:config.org::*Tweak =company-box=][Tweak =company-box=:1]]
(after! company-box
  (when (daemonp)
    (defun +company-box--reload-icons-h ()
      (setq company-box-icons-all-the-icons
            (let ((all-the-icons-scale-factor 0.8))
              `((Unknown       . ,(all-the-icons-faicon   "code"                 :face 'all-the-icons-purple))
                (Text          . ,(all-the-icons-material "text_fields"          :face 'all-the-icons-green))
                (Method        . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-red))
                (Function      . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-red))
                (Constructor   . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-red))
                (Field         . ,(all-the-icons-faicon   "tag"                  :face 'all-the-icons-red))
                (Variable      . ,(all-the-icons-material "adjust"               :face 'all-the-icons-blue))
                (Class         . ,(all-the-icons-material "class"                :face 'all-the-icons-red))
                (Interface     . ,(all-the-icons-material "tune"                 :face 'all-the-icons-red))
                (Module        . ,(all-the-icons-faicon   "cubes"                :face 'all-the-icons-red))
                (Property      . ,(all-the-icons-faicon   "wrench"               :face 'all-the-icons-red))
                (Unit          . ,(all-the-icons-material "straighten"           :face 'all-the-icons-red))
                (Value         . ,(all-the-icons-material "filter_1"             :face 'all-the-icons-red))
                (Enum          . ,(all-the-icons-material "plus_one"             :face 'all-the-icons-red))
                (Keyword       . ,(all-the-icons-material "filter_center_focus"  :face 'all-the-icons-red-alt))
                (Snippet       . ,(all-the-icons-faicon   "expand"               :face 'all-the-icons-red))
                (Color         . ,(all-the-icons-material "colorize"             :face 'all-the-icons-red))
                (File          . ,(all-the-icons-material "insert_drive_file"    :face 'all-the-icons-red))
                (Reference     . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
                (Folder        . ,(all-the-icons-material "folder"               :face 'all-the-icons-red-alt))
                (EnumMember    . ,(all-the-icons-material "people"               :face 'all-the-icons-red))
                (Constant      . ,(all-the-icons-material "pause_circle_filled"  :face 'all-the-icons-red))
                (Struct        . ,(all-the-icons-material "list"                 :face 'all-the-icons-red))
                (Event         . ,(all-the-icons-material "event"                :face 'all-the-icons-red))
                (Operator      . ,(all-the-icons-material "control_point"        :face 'all-the-icons-red))
                (TypeParameter . ,(all-the-icons-material "class"                :face 'all-the-icons-red))
                (Template      . ,(all-the-icons-material "settings_ethernet"    :face 'all-the-icons-green))
                (ElispFunction . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-red))
                (ElispVariable . ,(all-the-icons-material "adjust"               :face 'all-the-icons-blue))
                (ElispFeature  . ,(all-the-icons-material "stars"                :face 'all-the-icons-orange))
                (ElispFace     . ,(all-the-icons-material "format_paint"         :face 'all-the-icons-pink))))))

    ;; Replace Doom defined icons with mine
    (when (memq #'+company-box--load-all-the-icons server-after-make-frame-hook)
      (remove-hook 'server-after-make-frame-hook #'+company-box--load-all-the-icons))
    (add-hook 'server-after-make-frame-hook #'+company-box--reload-icons-h)))
;; Tweak =company-box=:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:2]]
(after! treemacs
  (require 'dired)

  ;; My custom stuff (from tecosaur's config)
  (setq +treemacs-file-ignore-extensions
        '(;; LaTeX
          "aux" "ptc" "fdb_latexmk" "fls" "synctex.gz" "toc"
          ;; LaTeX - bibliography
          "bbl"
          ;; LaTeX - glossary
          "glg" "glo" "gls" "glsdefs" "ist" "acn" "acr" "alg"
          ;; LaTeX - pgfplots
          "mw"
          ;; LaTeX - pdfx
          "pdfa.xmpi"
          ;; Python
          "pyc"))

  (setq +treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex"
          ;; Python
          "*/__pycache__"))

  ;; Reload treemacs theme
  (setq doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  (setq treemacs-show-hidden-files nil
        treemacs-hide-dot-git-directory t
        treemacs-width 30)

  (defvar +treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")

  (defvar +treemacs-file-ignore-globs '()
    "Globs which will are transformed to `+treemacs-file-ignore-regexps' which `+treemacs-ignore-filter' will ensure are ignored")

  (defvar +treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `+treeemacs-file-ignore-globs'")

  (defun +treemacs-file-ignore-generate-regexps ()
    "Generate `+treemacs-file-ignore-regexps' from `+treemacs-file-ignore-globs'"
    (setq +treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp +treemacs-file-ignore-globs)))

  (unless (equal +treemacs-file-ignore-globs '())
    (+treemacs-file-ignore-generate-regexps))

  (defun +treemacs-ignore-filter (file full-path)
    "Ignore files specified by `+treemacs-file-ignore-extensions', and `+treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) +treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp +treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))

  (add-to-list 'treemacs-ignored-file-predicates #'+treemacs-ignore-filter))
;; Treemacs:2 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
;; Run `M-x projectile-discover-projects-in-search-path' to reload paths from this variable
(setq projectile-project-search-path
      '("~/PhD/papers"
        "~/PhD/workspace"
        "~/PhD/workspace-no"
        "~/PhD/workspace-no/ez-wheel/swd-starter-kit-repo"
        ("~/Projects/foss" . 2))) ;; ("dir" . depth)

(setq projectile-ignored-projects
      '("/tmp"
        "~/"
        "~/.cache"
        "~/.doom.d"
        "~/.emacs.d/.local/straight/repos/"))

(setq +projectile-ignored-roots
      '("~/.cache"
        ;; No need for this one, as `doom-project-ignored-p' checks for files in `doom-local-dir'
        "~/.emacs.d/.local/straight/"))

(defun +projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `+projectile-ignored-roots'"
  (require 'cl-lib)
  (or (doom-project-ignored-p filepath) ;; Used by default by doom with `projectile-ignored-project-function'
      (cl-some (lambda (root) (file-in-directory-p (expand-file-name filepath) (expand-file-name root)))
          +projectile-ignored-roots)))

(setq projectile-ignored-project-function #'+projectile-ignored-project-function)
;; Projectile:1 ends here

;; [[file:config.org::*Tramp][Tramp:1]]
(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
;; Tramp:1 ends here

;; [[file:config.org::*Eros-eval][Eros-eval:1]]
(setq eros-eval-result-prefix "⟹ ")
;; Eros-eval:1 ends here

;; [[file:config.org::*=dir-locals.el=][=dir-locals.el=:1]]
(defun +dir-locals-reload-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (+dir-locals-reload-for-current-buffer))))))

(defun +dir-locals-enable-autoreload ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (message "Dir-locals will be reloaded after saving.")
    (add-hook 'after-save-hook '+dir-locals-reload-for-all-buffers-in-this-directory nil t)))

(add-hook! '(emacs-lisp-mode-hook lisp-data-mode-hook) #'+dir-locals-enable-autoreload)
;; =dir-locals.el=:1 ends here

;; [[file:config.org::*Eglot][Eglot:1]]
(after! eglot
  ;; A hack to make it works with projectile
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))

  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))

  ;; Use clangd with some options
  (set-eglot-client! 'c++-mode '("clangd" "-j=3" "--clang-tidy")))
;; Eglot:1 ends here

;; [[file:config.org::*Tweak UI][Tweak UI:1]]
(after! lsp-ui
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-log-io nil
        lsp-lens-enable t ; not working properly with ccls!
        lsp-diagnostics-provider :auto
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments '(symbols)))
;; Tweak UI:1 ends here

;; [[file:config.org::*LSP mode with =clangd=][LSP mode with =clangd=:1]]
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=4"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 1))
;; LSP mode with =clangd=:1 ends here

;; [[file:config.org::*Python][Python:1]]
(after! tramp
  (require 'lsp-mode)
  ;; (require 'lsp-pyright)

  (setq lsp-enable-snippet nil
        lsp-log-io nil
        ;; To bypass the "lsp--document-highlight fails if
        ;; textDocument/documentHighlight is not supported" error
        lsp-enable-symbol-highlighting nil)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "pyls")
    :major-modes '(python-mode)
    :remote? t
    :server-id 'pyls-remote)))
;; Python:1 ends here

;; [[file:config.org::*C/C++ with =clangd=][C/C++ with =clangd=:1]]
(after! tramp
  (require 'lsp-mode)

  (setq lsp-enable-snippet nil
        lsp-log-io nil
        ;; To bypass the "lsp--document-highlight fails if
        ;; textDocument/documentHighlight is not supported" error
        lsp-enable-symbol-highlighting nil)

  (lsp-register-client
    (make-lsp-client
     :new-connection
     (lsp-tramp-connection
      (lambda ()
        (cons "clangd-12" ; executable name on remote machine 'ccls'
              lsp-clients-clangd-args)))
     :major-modes '(c-mode c++-mode objc-mode cuda-mode)
     :remote? t
     :server-id 'clangd-remote)))
;; C/C++ with =clangd=:1 ends here

;; [[file:config.org::*VHDL][VHDL:1]]
(use-package! vhdl-mode
  :hook (vhdl-mode . #'+lsp-vhdl-ls-load)
  :init
  (defun +lsp-vhdl-ls-load ()
    (interactive)
    (lsp t)
    (flycheck-mode t))

  :config
  ;; Required unless vhdl_ls is on the $PATH
  (setq lsp-vhdl-server-path "~/Projects/foss/repos/rust_hdl/target/release/vhdl_ls"
        lsp-vhdl-server 'vhdl-ls
        lsp-vhdl--params nil)
  (require 'lsp-vhdl))
;; VHDL:1 ends here

;; [[file:config.org::*SonarLint][SonarLint:2]]
(use-package! lsp-sonarlint)
;; SonarLint:2 ends here

;; [[file:config.org::*Cppcheck][Cppcheck:1]]
(after! flycheck
  (setq flycheck-cppcheck-checks '("information"
                                   "missingInclude"
                                   "performance"
                                   "portability"
                                   "style"
                                   "unusedFunction"
                                   "warning"))) ;; Actually, we can use "all"
;; Cppcheck:1 ends here

;; [[file:config.org::*Project CMake][Project CMake:2]]
(use-package! project-cmake
    :config
    (require 'eglot)
    (project-cmake-scan-kits)
    (project-cmake-eglot-integration))
;; Project CMake:2 ends here

;; [[file:config.org::*Clang-format][Clang-format:2]]
(use-package! clang-format
  :when CLANG-FORMAT-P
  :commands (clang-format-region))
;; Clang-format:2 ends here

;; [[file:config.org::*Auto-include C++ headers][Auto-include C++ headers:2]]
(use-package! cpp-auto-include
  :commands cpp-auto-include)
;; Auto-include C++ headers:2 ends here

;; [[file:config.org::*Emacs Refactor][Emacs Refactor:2]]
(use-package! erefactor
  :defer t)
;; Emacs Refactor:2 ends here

;; [[file:config.org::*Lorem ipsum][Lorem ipsum:2]]
(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-sentences
             lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-list))
;; Lorem ipsum:2 ends here

;; [[file:config.org::*DAP][DAP:2]]
(after! dap-mode
  ;; Set latest versions
  (setq dap-cpptools-extension-version "1.11.5")
  (require 'dap-cpptools)

  (setq dap-codelldb-extension-version "1.7.4")
  (require 'dap-codelldb)

  (setq dap-gdb-lldb-extension-version "0.26.0")
  (require 'dap-gdb-lldb)

  ;; More minimal UI
  (setq dap-auto-configure-features '(breakpoints locals expressions tooltip)
        dap-auto-show-output nil ;; Hide the annoying server output
        lsp-enable-dap-auto-configure t)

  ;; Automatically trigger dap-hydra when a program hits a breakpoint.
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))

  ;; Automatically delete session and close dap-hydra when DAP is terminated.
  (add-hook 'dap-terminated-hook
            (lambda (arg)
              (call-interactively #'dap-delete-session)
              (dap-hydra/nil)))

  ;; A workaround to correctly show breakpoints
  ;; from: https://github.com/emacs-lsp/dap-mode/issues/374#issuecomment-1140399819
  (add-hook! +dap-running-session-mode
    (set-window-buffer nil (current-buffer))))
;; DAP:2 ends here

;; [[file:config.org::*Doom store][Doom store:1]]
(defun +debugger/clear-last-session ()
  "Clear the last stored session"
  (interactive)
  (doom-store-clear "+debugger"))

(map! :leader :prefix ("l" . "custom")
      (:when (modulep! :tools debugger +lsp)
       :prefix ("d" . "debugger")
       :desc "Clear last DAP session" "c" #'+debugger/clear-last-session))
;; Doom store:1 ends here

;; [[file:config.org::*Additional commands][Additional commands:1]]
(after! realgud
  (require 'hydra)

  ;; Add some missing gdb/rr commands
  (defun +realgud:cmd-start (arg)
    "start = break main + run"
    (interactive "p")
    (realgud-command "start"))

  (defun +realgud:cmd-reverse-next (arg)
    "Reverse next"
    (interactive "p")
    (realgud-command "reverse-next"))

  (defun +realgud:cmd-reverse-step (arg)
    "Reverse step"
    (interactive "p")
    (realgud-command "reverse-step"))

  (defun +realgud:cmd-reverse-continue (arg)
    "Reverse continue"
    (interactive "p")
    (realgud-command "reverse-continue"))

  (defun +realgud:cmd-reverse-finish (arg)
    "Reverse finish"
    (interactive "p")
    (realgud-command "reverse-finish"))

  ;; Define a hydra binding
  (defhydra realgud-hydra (:color pink :hint nil :foreign-keys run)
    "
 Stepping  |  _n_: next      |  _i_: step    |  _o_: finish  |  _c_: continue  |  _R_: restart  |  _u_: until-here
 Revese    | _rn_: next      | _ri_: step    | _ro_: finish  | _rc_: continue  |
 Breakpts  | _ba_: break     | _bD_: delete  | _bt_: tbreak  | _bd_: disable   | _be_: enable   | _tr_: backtrace
 Eval      | _ee_: at-point  | _er_: region  | _eE_: eval    |
           |  _!_: shell     | _Qk_: kill    | _Qq_: quit    | _Sg_: gdb       | _Ss_: start
"
    ("n"  realgud:cmd-next)
    ("i"  realgud:cmd-step)
    ("o"  realgud:cmd-finish)
    ("c"  realgud:cmd-continue)
    ("R"  realgud:cmd-restart)
    ("u"  realgud:cmd-until-here)
    ("rn" +realgud:cmd-reverse-next)
    ("ri" +realgud:cmd-reverse-step)
    ("ro" +realgud:cmd-reverse-finish)
    ("rc" +realgud:cmd-reverse-continue)
    ("ba" realgud:cmd-break)
    ("bt" realgud:cmd-tbreak)
    ("bD" realgud:cmd-delete)
    ("be" realgud:cmd-enable)
    ("bd" realgud:cmd-disable)
    ("ee" realgud:cmd-eval-at-point)
    ("er" realgud:cmd-eval-region)
    ("tr" realgud:cmd-backtrace)
    ("eE" realgud:cmd-eval)
    ("!"  realgud:cmd-shell)
    ("Qk" realgud:cmd-kill)
    ("Sg" realgud:gdb)
    ("Ss" +realgud:cmd-start)
    ("q"  nil "quit" :color blue) ;; :exit
    ("Qq" realgud:cmd-quit :color blue)) ;; :exit

  (defun +debugger/realgud:gdb-hydra ()
    "Run `realgud-hydra'."
    (interactive)
    (realgud-hydra/body))

  (map! :leader :prefix ("l" . "custom")
        (:when (modulep! :tools debugger)
         :prefix ("d" . "debugger")
         :desc "RealGUD hydra" "h" #'+debugger/realgud:gdb-hydra)))
;; Additional commands:1 ends here

;; [[file:config.org::*Record and replay =rr=][Record and replay =rr=:1]]
(after! realgud
  (defun +debugger/rr-replay ()
    "Launch `rr replay'."
    (interactive)
    (realgud:gdb (+str-replace "gdb" "rr replay" realgud:gdb-command-name)))

  (defun +debugger/rr-record ()
    "Launch `rr record' with parameters from launch.json or `+launch-json-debug-config'."
    (interactive)
    (let* ((conf (launch-json--config-choice))
           (args (launch-json--substite-special-vars (plist-get conf :program) (plist-get conf :args))))
      (unless (make-process :name "rr-record"
                            :buffer "*rr record*"
                            :command (append '("rr" "record") args))
        (message "Cannot start the 'rr record' process"))))

  (map! :leader :prefix ("l" . "custom")
        (:when (modulep! :tools debugger)
         :prefix ("d" . "debugger")
         :desc "rr record" "r" #'+debugger/rr-record
         :desc "rr replay" "R" #'+debugger/rr-replay)))
;; Record and replay =rr=:1 ends here

;; [[file:config.org::*Emacs GDB /a.k.a./ =gdb-mi=][Emacs GDB /a.k.a./ =gdb-mi=:2]]
(use-package! gdb-mi
  :init
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug)

  :config
  (setq gdb-window-setup-function #'gdb--setup-windows ;; TODO: Customize this
        gdb-ignore-gdbinit nil) ;; I use gdbinit to define some useful stuff
  ;; History
  (defvar +gdb-history-file "~/.gdb_history")
  (defun +gud-gdb-mode-hook-setup ()
    "GDB setup."

    ;; Suposes "~/.gdbinit" contains:
    ;; set history save on
    ;; set history filename ~/.gdb_history
    ;; set history remove-duplicates 2048
    (when (and (ring-empty-p comint-input-ring)
               (file-exists-p +gdb-history-file))
      (setq comint-input-ring-file-name +gdb-history-file)
      (comint-read-input-ring t)))

  (add-hook 'gud-gdb-mode-hook '+gud-gdb-mode-hook-setup))
;; Emacs GDB /a.k.a./ =gdb-mi=:2 ends here

;; [[file:config.org::*Custom layout for =gdb-many-windows=][Custom layout for =gdb-many-windows=:1]]
(setq gdb-many-windows nil)

(defun set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* ((w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height))) 'below))) ;; left bottom
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)))

(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let ((c-buffer (window-buffer (selected-window)))) ;; save current buffer
    ad-do-it
    (set-gdb-layout c-buffer)))

(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))
;; Custom layout for =gdb-many-windows=:1 ends here

;; [[file:config.org::*Highlight current line][Highlight current line:1]]
(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;; (move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer)))))

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)
;; Highlight current line:1 ends here

;; [[file:config.org::*WIP =launch.json= support for GUD and RealGUD][WIP =launch.json= support for GUD and RealGUD:1]]
;; A variable which to be used in .dir-locals.el, formatted as a list of plists;
;; '((:program "..." :args ("args1" "arg2" ...)))
(defvar +launch-json-debug-config nil)
;; WIP =launch.json= support for GUD and RealGUD:1 ends here

;; [[file:config.org::*WIP =launch.json= support for GUD and RealGUD][WIP =launch.json= support for GUD and RealGUD:4]]
(defvar launch-json--gud-debugger-regex
  (rx (seq bol (group-n 1 (or "gdb" "gud-gdb" "perldb" "pdb" "jdb" "guiler" "dbx" "sdb" "xdb") eol))))

(defvar launch-json--realgud-debugger-regex
  (rx (seq bol (or (seq "realgud:" (group-n 1 (or "gdb" "pdb"
                                                  "bashdb"  "kshdb" "zshd"
                                                  "perldb" "rdebug" "remake"
                                                  "trepan" "trepan2" "trepan3k" "trepanjs" "trepan.pl")))
                   (seq "realgud-" (group-n 1 (or "gub")))
                   ;; Additional debuggers
                   (seq "realgud:" (group-n 1 (or "xdebug" "pry" "jdb" "ipdb" "trepan-xpy" "trepan-ni" "node-inspect")))
                   ;; `realgud-lldb' defines the debug command as `realgud--lldb',
                   ;; We accept both `realgud:lldb' and `realgud--lldb' in the config
                   (seq "realgud" (or ":" "--") (group-n 1 (or "lldb")))) eol)))

;; Define aliases for realgud-lldb
(with-eval-after-load 'realgud-lldb
  (defalias 'realgud:lldb 'realgud--lldb)
  (defalias 'realgud:lldb-command-name 'realgud--lldb-command-name))

;; Define aliases for realgud-ipdb
(with-eval-after-load 'realgud-ipdb
  (defalias 'realgud:ipdb-command-name 'realgud--ipdb-command-name))

(defvar launch-json--last-config nil)

(defun launch-json-last-config-clear ()
  (interactive)
  (setq-local launch-json--last-config nil))

(defun launch-json--substite-special-vars (program &optional args)
  "Substitue variables in PROGRAM and ARGS.
Return a list, in which processed PROGRAM is the first element, followed by ARGS."
  (let* ((curr-file (ignore-errors (expand-file-name (buffer-file-name))))
         (ws-root (string-trim-right
                   (expand-file-name
                    (or (projectile-project-root)
                        (ignore-errors (file-name-directory curr-file))
                        "."))
                   "/"))
         (ws-basename (file-name-nondirectory ws-root)))
    ;; Replace special variables
    (mapcar
     (lambda (str)
       (+str-replace-all
        (append
         (list
          (cons "${workspaceFolder}" ws-root)
          (cons "${workspaceFolderBasename}" ws-basename)
          (cons "${userHome}" (or (getenv "HOME") (expand-file-name "~")))
          (cons "${pathSeparator}" (if (memq system-type
                                             '(windows-nt ms-dos cygwin))
                                       "\\" "/"))
          (cons "${selectedText}" (if (use-region-p)
                                      (buffer-substring-no-properties
                                       (region-beginning) (region-end)) "")))
         ;; To avoid problems if launched from a non-file buffer
         (when curr-file
           (list
            (cons "${file}" curr-file)
            (cons "${relativeFile}" (file-relative-name curr-file ws-root))
            (cons "${relativeFileDirname}" (file-relative-name
                                            (file-name-directory curr-file) ws-root))
            (cons "${fileBasename}" (file-name-nondirectory curr-file))
            (cons "${fileBasenameNoExtension}" (file-name-base curr-file))
            (cons "${fileDirname}" (file-name-directory curr-file))
            (cons "${fileExtname}" (file-name-extension curr-file))
            (cons "${lineNumber}" (line-number-at-pos (point) t)))))
        str))
     (cons program args))))

(defun launch-json--debugger-params (type)
  (let* ((front/backend
          (cond ((string-match launch-json--realgud-debugger-regex type)
                 (cons 'realgud (intern (match-string 1 type))))
                ((string-match launch-json--gud-debugger-regex type)
                 (cons 'gud (intern (match-string 1 type))))
                (t
                 (cons 'unknown 'unknown))))
         (frontend (car front/backend))
         (backend (cdr front/backend))
         (cmd-sym (unless (eq frontend 'unknown)
                    (intern (format (cond ((eq frontend 'gud) "gud-%s-%s")
                                          ((eq frontend 'realgud) "%s-%s")
                                          (t "%s-%s"))
                                    type
                                    "command-name")))))
    (message "[launch-json:params]: Found type: %s -> { frontend: %s | backend: %s }"
             type (symbol-name frontend) (symbol-name backend))
    (cond ((memq backend '(gud-gdb gdb))
           ;; Special case for '(gud . gdb), uses `gdb-mi'
           (let ((use-gdb-mi (equal front/backend '(gud . gdb))))
             `(:type ,type
               :debug-cmd ,(if use-gdb-mi 'gdb (intern type))
               :args-format " --args %s %s"
               :cmd ,cmd-sym
               :require ,(if use-gdb-mi 'gdb-mi frontend))))
          ((eq backend 'lldb)
           `(:type ,type
             :debug-cmd ,(intern type)
             :args-format " -- %s %s"
             :cmd ,cmd-sym
             :require ,(intern (if (eq frontend 'realgud)
                                   (+str-replace-all '(("--" . "-") (":" . "-")) type)
                                 type))))
          (t ;; TODO: to be expanded for each debugger
           `(:type ,type
             :debug-cmd ,(intern type)
             :args-format " %s %s"
             :cmd ,(if (equal front/backend '(realgud . ipdb)) 'realgud--ipdb-command-name cmd-sym)
             :require ,(cond ((equal front/backend '(realgud . trepan-ni)) 'realgud-trepan-ni)
                             (t frontend)))))))

(defun launch-json--debug-command (params debuggee-args)
  "Return the debug command for PARAMS with DEBUGGEE-ARGS."
  (when-let* ((prog (car debuggee-args))
              (cmd (plist-get params :cmd))
              (pkg (plist-get params :require)))
    (if (or (not pkg) (eq pkg 'unknown))
        (progn (message "[launch-json:command]: Unknown debugger")
               nil)
      (if (require (plist-get params :require) nil t)
          (let ((args (+str-join " " (cdr debuggee-args))))
            (when args (setq args (format (plist-get params :args-format) prog args)))
            (if (bound-and-true-p cmd)
                (concat (eval cmd) (if args args ""))
              (message "[launch-json:command]: Invalid command for type %s" (plist-get params :type))
              nil))
        (message "[launch-json:command]: Cannot add package %s" (symbol-name pkg))
        nil))))

(defun launch-json-read (&optional file)
  "Return the configurations section from a launch.json FILE.
If FILE is nil, launch.json will be searched in the current project,
if it is set to a launch.json file, it will be used instead."
  (let ((launch-json (expand-file-name (or file "launch.json") (or (projectile-project-root) "."))))
    (when (file-exists-p launch-json)
      (message "[launch-json]: Found \"launch.json\" at %s" launch-json)
      (let* ((launch (with-temp-buffer
                       (insert-file-contents launch-json)
                       (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil)))
             (configs (plist-get launch :configurations)))
        (+filter (lambda (conf)
                   (or (string-match-p launch-json--gud-debugger-regex (plist-get conf :type))
                       (string-match-p launch-json--realgud-debugger-regex (plist-get conf :type))))
                 configs)))))

(defun launch-json--config-choice (&optional file)
  (let* ((confs (or (launch-json-read file)
                    +launch-json-debug-config))
         (candidates (mapcar (lambda (conf)
                               (cons (format "%s [%s]" (plist-get conf :name) (plist-get conf :type))
                                     conf))
                             confs)))
    (cond ((eq (length confs) 1)
           (car confs))
          ((> (length confs) 1)
           (cdr (assoc (completing-read "Configuration: " candidates) candidates))))))

(defun launch-json-debug (&optional file)
  "Launch RealGUD or GDB with parameters from `+launch-json-debug-config' or launch.json file."
  (interactive)
  (let* ((conf (or launch-json--last-config
                   (launch-json--config-choice file)))
         (args (launch-json--substite-special-vars (plist-get conf :program) (plist-get conf :args)))
         (type (plist-get conf :type))
         (params (launch-json--debugger-params type)))
    (when params
      (let ((debug-cmd (plist-get params :debug-cmd)))
        (when (fboundp debug-cmd)
          (setq-local launch-json--last-config conf)
          (funcall debug-cmd
                   (launch-json--debug-command params args)))))))

(map! :leader :prefix ("l" . "custom")
      (:when (modulep! :tools debugger)
       :prefix ("d" . "debugger")
       :desc "GUD/RealGUD launch.json" "d" #'launch-json-debug))
;; WIP =launch.json= support for GUD and RealGUD:4 ends here

;; [[file:config.org::*Valgrind][Valgrind:2]]
(use-package! valgrind
  :commands valgrind)
;; Valgrind:2 ends here

;; [[file:config.org::*Emojify][Emojify:1]]
(setq emojify-emoji-set "twemoji-v2")
;; Emojify:1 ends here

;; [[file:config.org::*Emojify][Emojify:2]]
(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓" "⏱" "®" "™" "🅱" "❌" "✳"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))
;; Emojify:2 ends here

;; [[file:config.org::*Emojify][Emojify:3]]
(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))
;; Emojify:3 ends here

;; [[file:config.org::*Emojify][Emojify:4]]
(add-hook! '(mu4e-compose-mode org-msg-edit-mode circe-channel-mode) (emoticon-to-emoji 1))
;; Emojify:4 ends here

;; [[file:config.org::*Ligatures][Ligatures:1]]
(defun +appened-to-negation-list (head tail)
  (if (sequencep head)
    (delete-dups
     (if (eq (car tail) 'not)
         (append head tail)
       (append tail head)))
    tail))

(when (modulep! :ui ligatures)
  (setq +ligatures-extras-in-modes
        (+appened-to-negation-list
         +ligatures-extras-in-modes
         '(not c-mode c++-mode emacs-lisp-mode python-mode scheme-mode racket-mode rust-mode)))

  (setq +ligatures-in-modes
        (+appened-to-negation-list
         +ligatures-in-modes
         '(not emacs-lisp-mode scheme-mode racket-mode))))
;; Ligatures:1 ends here

;; [[file:config.org::*Spell-Fu][Spell-Fu:1]]
(after! spell-fu
  (defun +spell-fu-register-dictionary (lang)
    "Add `LANG` to spell-fu multi-dict, with a personal dictionary."
    ;; Add the dictionary
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary lang))
    (let ((personal-dict-file (expand-file-name (format "aspell.%s.pws" lang) doom-user-dir)))
      ;; Create an empty personal dictionary if it doesn't exists
      (unless (file-exists-p personal-dict-file) (write-region "" nil personal-dict-file))
      ;; Add the personal dictionary
      (spell-fu-dictionary-add (spell-fu-get-personal-dictionary (format "%s-personal" lang) personal-dict-file))))

  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (+spell-fu-register-dictionary +my/lang-main)
              (+spell-fu-register-dictionary +my/lang-secondary))))
;; Spell-Fu:1 ends here

;; [[file:config.org::*Guess language][Guess language:2]]
(use-package! guess-language
  :config
  (setq guess-language-languages '(en fr ar)
        guess-language-min-paragraph-length 35
        guess-language-langcodes '((en . ("en_US"    "English" "🇺🇸" "English"))
                                   (fr . ("francais" "French"  "🇫🇷" "Français"))
                                   (ar . ("arabic"   "Arabic"  "🇩🇿" "Arabic"))))
  ;; :hook (text-mode . guess-language-mode)
  :commands (guess-language
             guess-language-mode
             guess-language-region
             guess-language-mark-lines))
;; Guess language:2 ends here

;; [[file:config.org::*Grammarly][Grammarly:2]]
(use-package! grammarly
  :config
  (grammarly-load-from-authinfo))
;; Grammarly:2 ends here

;; [[file:config.org::*Eglot][Eglot:2]]
(use-package! eglot-grammarly
  :when (modulep! :tools lsp +eglot)
  :commands (+lsp-grammarly-load)
  :init
  (defun +lsp-grammarly-load ()
    "Load Grammarly LSP server for Eglot."
    (interactive)
    (require 'eglot-grammarly)
    (call-interactively #'eglot)))
;; Eglot:2 ends here

;; [[file:config.org::*LSP Mode][LSP Mode:2]]
(use-package! lsp-grammarly
  :when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
  :commands (+lsp-grammarly-load +lsp-grammarly-toggle)
  :init
  (defun +lsp-grammarly-load ()
    "Load Grammarly LSP server for LSP Mode."
    (interactive)
    (require 'lsp-grammarly)
    (lsp-deferred)) ;; or (lsp)

  (defun +lsp-grammarly-enabled-p ()
    (not (member 'grammarly-ls lsp-disabled-clients)))

  (defun +lsp-grammarly-enable ()
    "Enable Grammarly LSP."
    (interactive)
    (when (not (+lsp-grammarly-enabled-p))
      (setq lsp-disabled-clients (remove 'grammarly-ls lsp-disabled-clients))
      (message "Enabled grammarly-ls"))
    (+lsp-grammarly-load))

  (defun +lsp-grammarly-disable ()
    "Disable Grammarly LSP."
    (interactive)
    (when (+lsp-grammarly-enabled-p)
      (add-to-list 'lsp-disabled-clients 'grammarly-ls)
      (lsp-disconnect)
      (message "Disabled grammarly-ls")))

  (defun +lsp-grammarly-toggle ()
    "Enable/disable Grammarly LSP."
    (interactive)
    (if (+lsp-grammarly-enabled-p)
        (+lsp-grammarly-disable)
      (+lsp-grammarly-enable)))

  (after! lsp-mode
    ;; Disable by default
    (add-to-list 'lsp-disabled-clients 'grammarly-ls))

  :config
  (set-lsp-priority! 'grammarly-ls 1))
;; LSP Mode:2 ends here

;; [[file:config.org::*Grammalecte][Grammalecte:2]]
(use-package! flycheck-grammalecte
  :commands (flycheck-grammalecte-correct-error-at-point
             grammalecte-conjugate-verb
             grammalecte-define
             grammalecte-define-at-point
             grammalecte-find-synonyms
             grammalecte-find-synonyms-at-point)
  :init
  (setq grammalecte-settings-file (expand-file-name "grammalecte/grammalecte-cache.el" doom-data-dir)
        grammalecte-python-package-directory (expand-file-name "grammalecte/grammalecte" doom-data-dir))

  (setq flycheck-grammalecte-report-spellcheck t
        flycheck-grammalecte-report-grammar t
        flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp nil
        flycheck-grammalecte-report-nbsp nil
        flycheck-grammalecte-filters
        '("(?m)^# ?-*-.+$"
          ;; Ignore LaTeX equations (inline and block)
          "\\$.*?\\$"
          "(?s)\\\\begin{\\(?1:\\(?:equation.\\|align.\\)\\)}.*?\\\\end{\\1}"))

  (map! :leader :prefix ("l" . "custom")
        (:prefix ("g" . "grammalecte")
         :desc "Correct error at point"     "p" #'flycheck-grammalecte-correct-error-at-point
         :desc "Conjugate a verb"           "V" #'grammalecte-conjugate-verb
         :desc "Define a word"              "W" #'grammalecte-define
         :desc "Conjugate a verb at point"  "w" #'grammalecte-define-at-point
         :desc "Find synonyms"              "S" #'grammalecte-find-synonyms
         :desc "Find synonyms at point"     "s" #'grammalecte-find-synonyms-at-point))

  :config
  (grammalecte-download-grammalecte)
  (flycheck-grammalecte-setup)
  (add-to-list 'flycheck-grammalecte-enabled-modes 'fountain-mode))
;; Grammalecte:2 ends here

;; [[file:config.org::*LanguageTool Server][LanguageTool Server:1]]
(when LANGUAGETOOL-P
  (defun +languagetool-server-running-p ()
    (and LANGUAGETOOL-P
         (+systemd-running-p "languagetool")))

  (defun +languagetool-server-start ()
    "Start LanguageTool server with PORT."
    (interactive)
    (unless (+languagetool-server-running-p)
      (+systemd-start "languagetool")))

  (defun +languagetool-server-stop ()
    "Stop the LanguageTool server."
    (interactive)
    (if (+languagetool-server-running-p)
        (progn
          (+systemd-stop "languagetool")
          (message "Stopped LanguageTool server.")))
    (message "No LanguageTool server running."))

  (defun +languagetool-server-restart ()
    (interactive)
    (when (+languagetool-server-running-p)
      (+systemd-command "languagetool" "restart"))))

(map! :leader :prefix ("l" . "custom")
      (:when LANGUAGETOOL-P
       :prefix ("l" . "languagetool")
       (:prefix ("s" . "server")
        :desc "Start server"     "s" #'+languagetool-server-start
        :desc "Stop server"      "q" #'+languagetool-server-stop
        :desc "Restart server"   "r" #'+languagetool-server-restart)))
;; LanguageTool Server:1 ends here

;; [[file:config.org::*LTeX][LTeX:2]]
;; NOTE To be removed by 1 Sep 2022,
;; after https://github.com/doomemacs/doomemacs/pull/6683 gets merged
(use-package! lsp-ltex
  :when (modulep! :checkers grammar +lsp)
  :unless (modulep! :tools lsp +eglot)
  :commands (+lsp-ltex-toggle
             +lsp-ltex-enable
             +lsp-ltex-disable
             +lsp-ltex-setup)
  :hook ((text-mode latex-mode org-mode markdown-mode) . #'+lsp-ltex-setup)
  :config
  ;; Disable by default, can be enabled in a ber buffer (or workspace) basis
  (add-to-list 'lsp-disabled-clients 'ltex-ls)
  :init
  (setq lsp-ltex-check-frequency "save" ;; Less overhead than the default "edit"
        lsp-ltex-log-level "warning" ;; No need to log everything
        ;; Path in which, interactively added words and rules will be stored.
        lsp-ltex-user-rules-path (expand-file-name "lsp-ltex" doom-data-dir))

  (unless (+languagetool-server-running-p)
    (+languagetool-server-restart)
    (sit-for 5))

  (when (+languagetool-server-running-p)
    (setq lsp-ltex-languagetool-http-server-uri "http://localhost:8081"))

  ;; When n-gram data sets are available, use them to detect errors with words
  ;; that are often confused (like their and there).
  (when (file-directory-p "/usr/share/ngrams")
    (setq lsp-ltex-additional-rules-language-model "/usr/share/ngrams"))

  (defun +lsp-ltex-setup ()
    "Load LTeX LSP server."
    (interactive)
    (require 'lsp-ltex)
    (when (+lsp-ltex--enabled-p)
      (lsp-deferred)))

  (defun +lsp-ltex--enabled-p ()
    (not (memq 'ltex-ls lsp-disabled-clients)))

  (defun +lsp-ltex-enable ()
    "Enable LTeX LSP for the current buffer."
    (interactive)
    (unless (+lsp-ltex--enabled-p)
      (setq-local lsp-disabled-clients (delq 'ltex-ls lsp-disabled-clients))
      (message "Enabled ltex-ls"))
    (+lsp-ltex-setup))

  (defun +lsp-ltex-disable ()
    "Disable LTeX LSP for the current buffer."
    (interactive)
    (when (+lsp-ltex--enabled-p)
      (setq-local lsp-disabled-clients (cons 'ltex-ls lsp-disabled-clients))
      (lsp-disconnect)
      (message "Disabled ltex-ls")))

  (defun +lsp-ltex-toggle ()
    "Toggle LTeX LSP for the current buffer."
    (interactive)
    (if (+lsp-ltex--enabled-p)
        (+lsp-ltex-disable)
      (+lsp-ltex-enable)))

  (map! :localleader
        :map (text-mode-map latex-mode-map org-mode-map markdown-mode-map)
        :desc "Toggle grammar check" "G" #'+lsp-ltex-toggle))

(after! lsp-ltex
  (add-to-list 'lsp-disabled-clients 'ltex-ls)
  (setq lsp-ltex-language "auto"
        lsp-ltex-mother-tongue +my/lang-mother-tongue
        flycheck-checker-error-threshold 1000))
;; LTeX:2 ends here

;; [[file:config.org::*Flycheck][Flycheck:2]]
(use-package! flycheck-languagetool
  :when LANGUAGETOOL-P
  :commands (+flycheck-languagetool-setup-locally)
  :hook (org-msg-edit-mode . +flycheck-languagetool-setup-locally)
  :init
  (setq flycheck-languagetool-url "http://localhost:8081"
        flycheck-languagetool-server-port 8081
        flycheck-languagetool-language "auto"
        ;; See https://languagetool.org/http-api/swagger-ui/#!/default/post_check
        flycheck-languagetool-check-params
        `(("disabledRules" . "FRENCH_WHITESPACE,WHITESPACE,DEUX_POINTS_ESPACE")
          ("motherTongue"  . ,+my/lang-mother-tongue)))

  (defun +flycheck-languagetool-setup-locally ()
    "Setup flycheck-languagetool locally."
    (interactive)
    (setq-local flycheck-checkers
                (append '(languagetool) flycheck-checkers))))
;; Flycheck:2 ends here

;; [[file:config.org::*Go Translate (Google, Bing and DeepL)][Go Translate (Google, Bing and DeepL):2]]
(use-package! go-translate
  :commands (gts-do-translate
             +gts-yank-translated-region
             +gts-translate-with)
  :init
  ;; Your languages pairs
  (setq gts-translate-list (list (list +my/lang-main +my/lang-secondary)
                                 (list +my/lang-main +my/lang-mother-tongue)
                                 (list +my/lang-secondary +my/lang-mother-tongue)
                                 (list +my/lang-secondary +my/lang-main)))

  (map! :localleader
        :map (org-mode-map markdown-mode-map latex-mode-map text-mode-map)
        :desc "Yank translated region" "R" #'+gts-yank-translated-region)

  (map! :leader :prefix "l"
        (:prefix ("G" . "go-translate")
         :desc "Bing"                   "b" (lambda () (interactive) (+gts-translate-with 'bing))
         :desc "DeepL"                  "d" (lambda () (interactive) (+gts-translate-with 'deepl))
         :desc "Google"                 "g" (lambda () (interactive) (+gts-translate-with))
         :desc "Yank translated region" "R" #'+gts-yank-translated-region
         :desc "gts-do-translate"       "t" #'gts-do-translate))

  :config
  ;; Config the default translator, which will be used by the command `gts-do-translate'
  (setq gts-default-translator
        (gts-translator
         ;; Used to pick source text, from, to. choose one.
         :picker (gts-prompt-picker)
         ;; One or more engines, provide a parser to give different output.
         :engines (gts-google-engine :parser (gts-google-summary-parser))
         ;; Render, only one, used to consumer the output result.
         :render (gts-buffer-render)))

  ;; Custom texter which remove newlines in the same paragraph
  (defclass +gts-translate-paragraph (gts-texter) ())

  (cl-defmethod gts-text ((_ +gts-translate-paragraph))
    (when (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (while (re-search-forward "\n[^\n]" nil t)
              (replace-region-contents
               (- (point) 2) (- (point) 1)
               (lambda (&optional a b) " ")))
            (buffer-string))))))

  ;; Custom picker to use the paragraph texter
  (defclass +gts-paragraph-picker (gts-picker)
    ((texter :initarg :texter :initform (+gts-translate-paragraph))))

  (cl-defmethod gts-pick ((o +gts-paragraph-picker))
    (let ((text (gts-text (oref o texter))))
      (when (or (null text) (zerop (length text)))
        (user-error "Make sure there is any word at point, or selection exists"))
      (let ((path (gts-path o text)))
        (setq gts-picker-current-path path)
        (cl-values text path))))

  (defun +gts-yank-translated-region ()
    (interactive)
    (gts-translate
     (gts-translator
      :picker (+gts-paragraph-picker)
      :engines (gts-google-engine)
      :render (gts-kill-ring-render))))

  (defun +gts-translate-with (&optional engine)
    (interactive)
    (gts-translate
     (gts-translator
      :picker (+gts-paragraph-picker)
      :engines
      (cond ((eq engine 'deepl)
             (gts-deepl-engine
              :auth-key ;; Get API key from ~/.authinfo.gpg (machine api-free.deepl.com)
              (funcall
               (plist-get (car (auth-source-search :host "api-free.deepl.com" :max 1))
                          :secret))
              :pro nil))
            ((eq engine 'bing) (gts-bing-engine))
            (t (gts-google-engine)))
      :render (gts-buffer-render)))))
;; Go Translate (Google, Bing and DeepL):2 ends here

;; [[file:config.org::*Disk usage][Disk usage:2]]
(use-package! disk-usage
  :commands (disk-usage))
;; Disk usage:2 ends here

;; [[file:config.org::*Chezmoi][Chezmoi:2]]
(use-package! chezmoi
  :when CHEZMOI-P
  :commands (chezmoi-write
             chezmoi-magit-status
             chezmoi-diff
             chezmoi-ediff
             chezmoi-find
             chezmoi-write-files
             chezmoi-open-other
             chezmoi-template-buffer-display
             chezmoi-mode)
  :config
  ;; Company integration
  (when (modulep! :completion company)
    (defun +chezmoi--company-backend-h ()
      (require 'chezmoi-company)
      (if chezmoi-mode
          (add-to-list 'company-backends 'chezmoi-company-backend)
        (delete 'chezmoi-company-backend 'company-backends)))

    (add-hook 'chezmoi-mode-hook #'+chezmoi--company-backend-h))

  ;; Integrate with evil mode by toggling template display when entering insert mode.
  (when (modulep! :editor evil)
    (defun +chezmoi--evil-insert-state-enter-h ()
      "Run after evil-insert-state-entry."
      (chezmoi-template-buffer-display nil (point))
      (remove-hook 'after-change-functions #'chezmoi-template--after-change 1))

    (defun +chezmoi--evil-insert-state-exit-h ()
      "Run after evil-insert-state-exit."
      (chezmoi-template-buffer-display nil)
      (chezmoi-template-buffer-display t)
      (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1))

    (defun +chezmoi--evil-h ()
      (if chezmoi-mode
          (progn
            (add-hook 'evil-insert-state-entry-hook #'+chezmoi--evil-insert-state-enter-h nil 1)
            (add-hook 'evil-insert-state-exit-hook #'+chezmoi--evil-insert-state-exit-h nil 1))
        (progn
          (remove-hook 'evil-insert-state-entry-hook #'+chezmoi--evil-insert-state-enter-h 1)
          (remove-hook 'evil-insert-state-exit-hook #'+chezmoi--evil-insert-state-exit-h 1))))

    (add-hook 'chezmoi-mode-hook #'+chezmoi--evil-h)))

(map! :leader :prefix ("l" . "custom")
      (:prefix ("t" . "tools")
       (:when CHEZMOI-P
        :prefix ("c" . "chezmoi")
        :desc "Magit status" "g" #'chezmoi-magit-status
        :desc "Write"        "w" #'chezmoi-write
        :desc "Write files"  "W" #'chezmoi-write-files
        :desc "Find source"  "f" #'chezmoi-find
        :desc "Sync files"   "s" #'chezmoi-sync-files
        :desc "Diff"         "d" #'chezmoi-diff
        :desc "EDiff"        "e" #'chezmoi-ediff
        :desc "Open other"   "o" #'chezmoi-open-other)))
;; Chezmoi:2 ends here

;; [[file:config.org::*Aweshell][Aweshell:2]]
(use-package! aweshell
  :commands (aweshell-new aweshell-dedicated-open))
;; Aweshell:2 ends here

;; [[file:config.org::*Lemon][Lemon:2]]
(use-package! lemon
  :commands (lemon-mode lemon-display)
  :config
  (require 'lemon-cpu)
  (require 'lemon-memory)
  (require 'lemon-network)
  (setq lemon-delay 5
        lemon-refresh-rate 2
        lemon-monitors
        (list '((lemon-cpufreq-linux :display-opts '(:sparkline (:type gridded)))
                (lemon-cpu-linux)
                (lemon-memory-linux)
                (lemon-linux-network-tx)
                (lemon-linux-network-rx)))))
;; Lemon:2 ends here

;; [[file:config.org::*eCryptfs][eCryptfs:1]]
(when ECRYPTFS-P
  (defvar +ecryptfs-private-dir "Private")
  (defvar +ecryptfs-buffer-name "*emacs-ecryptfs*")
  (defvar +ecryptfs-config-dir (expand-file-name "~/.ecryptfs"))
  (defvar +ecryptfs-passphrase-gpg (expand-file-name "~/.ecryptfs/my-pass.gpg"))
  (defvar +ecryptfs--wrapping-independent-p (not (null (expand-file-name "wrapping-independent" +ecryptfs-config-dir))))
  (defvar +ecryptfs--wrapped-passphrase-file (expand-file-name "wrapped-passphrase" +ecryptfs-config-dir))
  (defvar +ecryptfs--mount-passphrase-sig-file (concat (expand-file-name +ecryptfs-private-dir +ecryptfs-config-dir) ".sig"))
  (defvar +ecryptfs--mount-private-cmd "/sbin/mount.ecryptfs_private")
  (defvar +ecryptfs--umount-private-cmd "/sbin/umount.ecryptfs_private")
  (defvar +ecryptfs--passphrase
    (lambda ()
      (s-trim-right ;; To remove the new line
       (epg-decrypt-file (epg-make-context)
                         +ecryptfs-passphrase-gpg
                         nil))))
  (defvar +ecryptfs--encrypt-filenames-p
    (not (eq 1
             (with-temp-buffer
               (insert-file-contents +ecryptfs--mount-passphrase-sig-file)
               (count-lines (point-min) (point-max))))))
  (defvar +ecryptfs--command-format
    (if +ecryptfs--encrypt-filenames-p
        "ecryptfs-insert-wrapped-passphrase-into-keyring %s '%s'"
      "ecryptfs-unwrap-passphrase %s '%s' | ecryptfs-add-passphrase -"))

  (defun +ecryptfs-mount-private ()
    (interactive)
    (unless (and (file-exists-p +ecryptfs--wrapped-passphrase-file)
                 (file-exists-p +ecryptfs--mount-passphrase-sig-file))
      (error "Encrypted private directory \"%s\" is not setup properly."
             +ecryptfs-private-dir)
      (return))

    (let ((try-again t))
      (while (and
              ;; In the first iteration, we try to silently mount the ecryptfs private directory,
              ;; this would succeed if the key is available in the keyring.
              (shell-command +ecryptfs--mount-private-cmd
                             +ecryptfs-buffer-name)
              try-again)
        (setq try-again nil)
        (message "Encrypted filenames mode [%s]." (if +ecryptfs--encrypt-filenames-p "ENABLED" "DISABLED"))
        (shell-command
         (format +ecryptfs--command-format
                 +ecryptfs--wrapped-passphrase-file
                 (funcall +ecryptfs--passphrase))
         +ecryptfs-buffer-name))
      (message "Ecryptfs mount private.")))

  (defun +ecryptfs-umount-private ()
    (interactive)
    (while (string-match-p "Sessions still open, not unmounting"
                           (shell-command-to-string +ecryptfs--umount-private-cmd)))
    (message "Unmounted private directory.")))

(map! :leader :prefix ("l" . "custom")
      (:prefix ("t" . "tools")
       (:when ECRYPTFS-P
        :prefix ("e" . "ecryptfs")
        :desc "eCryptfs mount private"    "e" #'+ecryptfs-mount-private
        :desc "eCryptfs un-mount private" "E" #'+ecryptfs-umount-private)))
;; eCryptfs:1 ends here

;; [[file:config.org::*Workspaces][Workspaces:1]]
(map! :leader
      (:when (modulep! :ui workspaces)
       :prefix ("TAB" . "workspace")
       :desc "Display tab bar"           "TAB" #'+workspace/display
       :desc "Switch workspace"          "."   #'+workspace/switch-to
       :desc "Switch to last workspace"  "$"   #'+workspace/other ;; Modified
       :desc "New workspace"             "n"   #'+workspace/new
       :desc "New named workspace"       "N"   #'+workspace/new-named
       :desc "Load workspace from file"  "l"   #'+workspace/load
       :desc "Save workspace to file"    "s"   #'+workspace/save
       :desc "Delete session"            "x"   #'+workspace/kill-session
       :desc "Delete this workspace"     "d"   #'+workspace/delete
       :desc "Rename workspace"          "r"   #'+workspace/rename
       :desc "Restore last session"      "R"   #'+workspace/restore-last-session
       :desc "Next workspace"            ">"   #'+workspace/switch-right ;; Modified
       :desc "Previous workspace"        "<"   #'+workspace/switch-left ;; Modified
       :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
       :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
       :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
       :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
       :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
       :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
       :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
       :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
       :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
       :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))
;; Workspaces:1 ends here

;; [[file:config.org::*Weather][Weather:2]]
(use-package! wttrin
  :commands wttrin)
;; Weather:2 ends here

;; [[file:config.org::*OpenStreetMap][OpenStreetMap:2]]
(use-package! osm
  :commands (osm-home
             osm-search
             osm-server
             osm-goto
             osm-gpx-show
             osm-bookmark-jump)

  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information

  :init
  (setq osm-tile-directory (expand-file-name "osm" doom-data-dir))
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))
;; OpenStreetMap:2 ends here

;; [[file:config.org::*Islamic prayer times][Islamic prayer times:2]]
(use-package! awqat
  :commands (awqat-display-prayer-time-mode awqat-times-for-day)
  :config
  ;; Make sure `calendar-latitude' and `calendar-longitude' are set,
  ;; otherwise, set them here.
  (setq awqat-asr-hanafi nil
        awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) ")
  (awqat-set-preset-french-muslims))
;; Islamic prayer times:2 ends here

;; [[file:config.org::*Info colors][Info colors:2]]
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
;; Info colors:2 ends here

;; [[file:config.org::*Zotero Zotxt][Zotero Zotxt:2]]
(use-package! zotxt
  :when ZOTERO-P
  :commands org-zotxt-mode)
;; Zotero Zotxt:2 ends here

;; [[file:config.org::*CRDT][CRDT:2]]
(use-package! crdt
  :commands (crdt-share-buffer
             crdt-connect
             crdt-visualize-author-mode
             crdt-org-sync-overlay-mode))
;; CRDT:2 ends here

;; [[file:config.org::*The Silver Searcher][The Silver Searcher:2]]
(use-package! ag
  :when AG-P
  :commands (ag
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp))
;; The Silver Searcher:2 ends here

;; [[file:config.org::*Page break lines][Page break lines:2]]
(use-package! page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
;; Page break lines:2 ends here

;; [[file:config.org::*Emacs Application Framework][Emacs Application Framework:1]]
(use-package! eaf
  :when EAF-P
  :load-path EAF-DIR
  :commands (eaf-open
             eaf-open-browser
             eaf-open-jupyter
             +eaf-open-mail-as-html)
  :init
  (defvar +eaf-enabled-apps
    '(org browser mindmap jupyter org-previewer markdown-previewer file-sender video-player))

  (defun +eaf-app-p (app-symbol)
    (memq app-symbol +eaf-enabled-apps))

  (when (+eaf-app-p 'browser)
    ;; Make EAF Browser my default browser
    (setq browse-url-browser-function #'eaf-open-browser)
    (defalias 'browse-web #'eaf-open-browser)

    (map! :localleader
          :map (mu4e-headers-mode-map mu4e-view-mode-map)
          :desc "Open mail as HTML" "h" #'+eaf-open-mail-as-html
          :desc "Open URL (EAF)" "o" #'eaf-open-browser))

  (when (+eaf-app-p 'pdf-viewer)
    (after! org
      ;; Use EAF PDF Viewer in Org
      (defun +eaf--org-open-file-fn (file &optional link)
        "An wrapper function on `eaf-open'."
        (eaf-open file))

      ;; use `emacs-application-framework' to open PDF file: link
      (add-to-list 'org-file-apps '("\\.pdf\\'" . +eaf--org-open-file-fn)))

    (after! latex
      ;; Link EAF with the LaTeX compiler in emacs. When a .tex file is open,
      ;; the Command>Compile and view (C-c C-a) option will compile the .tex
      ;; file into a .pdf file and display it using EAF. Double clicking on the
      ;; PDF side jumps to editing the clicked section.
      (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
      (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
      (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

  :config
  ;; Generic
  (setq eaf-start-python-process-when-require t
        eaf-kill-process-after-last-buffer-closed t
        eaf-fullscreen-p nil)

  ;; Debug
  (setq eaf-enable-debug nil)

  ;; Web engine
  (setq eaf-webengine-font-family (symbol-name (font-get doom-font :family))
        eaf-webengine-fixed-font-family (symbol-name (font-get doom-font :family))
        eaf-webengine-serif-font-family (symbol-name (font-get doom-serif-font :family))
        eaf-webengine-font-size 16
        eaf-webengine-fixed-font-size 16
        eaf-webengine-enable-scrollbar t
        eaf-webengine-scroll-step 200
        eaf-webengine-default-zoom 1.25
        eaf-webengine-show-hover-link t
        eaf-webengine-download-path "~/Downloads"
        eaf-webengine-enable-plugin t
        eaf-webengine-enable-javascript t
        eaf-webengine-enable-javascript-access-clipboard t)

  (when (display-graphic-p)
    (require 'eaf-all-the-icons)
    (mapc (lambda (v) (eaf-all-the-icons-icon (car v)))
          eaf-all-the-icons-alist))

  ;; Browser settings
  (when (+eaf-app-p 'browser)
    (setq eaf-browser-continue-where-left-off t
          eaf-browser-dark-mode nil ;; "follow"
          eaf-browser-enable-adblocker t
          eaf-browser-enable-autofill nil
          eaf-browser-remember-history t
          eaf-browser-ignore-history-list '("google.com/search" "file://")
          eaf-browser-text-selection-color "auto"
          eaf-browser-translate-language +my/lang-main
          eaf-browser-blank-page-url "https://www.duckduckgo.com"
          eaf-browser-chrome-history-file "~/.config/google-chrome/Default/History"
          eaf-browser-default-search-engine "duckduckgo"
          eaf-browser-continue-where-left-off t
          eaf-browser-aria2-auto-file-renaming t)

    (require 'eaf-browser)

    (defun +eaf-open-mail-as-html ()
      "Open the html mail in EAF Browser."
      (interactive)
      (let ((msg (mu4e-message-at-point t))
            ;; Bind browse-url-browser-function locally, so it works
            ;; even if EAF Browser is not set as a default browser.
            (browse-url-browser-function #'eaf-open-browser))
        (if msg
            (mu4e-action-view-in-browser msg)
          (message "No message at point.")))))

  ;; File manager settings
  (when (+eaf-app-p 'file-manager)
    (setq eaf-file-manager-show-preview nil
          eaf-find-alternate-file-in-dired t
          eaf-file-manager-show-hidden-file t
          eaf-file-manager-show-icon t)
    (require 'eaf-file-manager))

  ;; File Browser
  (when (+eaf-app-p 'file-browser)
    (require 'eaf-file-browser))

  ;; PDF Viewer settings
  (when (+eaf-app-p 'pdf-viewer)
    (setq eaf-pdf-dark-mode "follow"
          eaf-pdf-show-progress-on-page nil
          eaf-pdf-dark-exclude-image t
          eaf-pdf-notify-file-changed t)
    (require 'eaf-pdf-viewer))

  ;; Org
  (when (+eaf-app-p 'rss-reader)
    (setq eaf-rss-reader-split-horizontally nil
          eaf-rss-reader-web-page-other-window t)
    (require 'eaf-org))

  ;; Org
  (when (+eaf-app-p 'org)
    (require 'eaf-org))

  ;; Mail
  ;; BUG The `eaf-open-mail-as-html' is not working,
  ;;     I use `+eaf-open-mail-as-html' instead
  (when (+eaf-app-p 'mail)
    (require 'eaf-mail))

  ;; Org Previewer
  (when (+eaf-app-p 'org-previewer)
    (setq eaf-org-dark-mode "follow")
    (require 'eaf-org-previewer))

  ;; Markdown Previewer
  (when (+eaf-app-p 'markdown-previewer)
    (setq eaf-markdown-dark-mode "follow")
    (require 'eaf-markdown-previewer))

  ;; Jupyter
  (when (+eaf-app-p 'jupyter)
    (setq eaf-jupyter-dark-mode "follow"
          eaf-jupyter-font-family (symbol-name (font-get doom-font :family))
          eaf-jupyter-font-size 13)
    (require 'eaf-jupyter))

  ;; Mindmap
  (when (+eaf-app-p 'mindmap)
    (setq eaf-mindmap-dark-mode "follow"
          eaf-mindmap-save-path "~/Dropbox/Mindmap")
    (require 'eaf-mindmap))

  ;; File Sender
  (when (+eaf-app-p 'file-sender)
    (require 'eaf-file-sender))

  ;; Music Player
  (when (+eaf-app-p 'music-player)
    (require 'eaf-music-player))

  ;; Video Player
  (when (+eaf-app-p 'video-player)
    (setq eaf-video-player-keybinding
          '(("p" . "toggle_play")
            ("q" . "close_buffer")
            ("h" . "play_backward")
            ("l" . "play_forward")
            ("j" . "decrease_volume")
            ("k" . "increase_volume")
            ("f" . "toggle_fullscreen")
            ("R" . "restart")))
    (require 'eaf-video-player))

  ;; Image Viewer
  (when (+eaf-app-p 'image-viewer)
    (require 'eaf-image-viewer))

  ;; Git
  (when (+eaf-app-p 'git)
    (require 'eaf-git))

  ;; Fix EVIL keybindings
  (after! evil
    (require 'eaf-evil)
    (define-key key-translation-map (kbd "SPC")
      (lambda (prompt)
        (if (derived-mode-p 'eaf-mode)
            (pcase eaf--buffer-app-name
              ("browser" (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                             (kbd "SPC")
                           (kbd eaf-evil-leader-key)))
              ("pdf-viewer" (kbd eaf-evil-leader-key))
              ("image-viewer" (kbd eaf-evil-leader-key))
              ("music-player" (kbd eaf-evil-leader-key))
              ("video-player" (kbd eaf-evil-leader-key))
              ("file-sender" (kbd eaf-evil-leader-key))
              ("mindmap" (kbd eaf-evil-leader-key))
              (_  (kbd "SPC")))
          (kbd "SPC"))))))
;; Emacs Application Framework:1 ends here

;; [[file:config.org::*Bitwarden][Bitwarden:2]]
(use-package! bitwarden
  ;;:config
  ;;(bitwarden-auth-source-enable)
  :when BITWARDEN-P
  :init
  (setq bitwarden-automatic-unlock
        (lambda ()
          (require 'auth-source)
          (if-let* ((matches (auth-source-search :host "bitwarden.com" :max 1))
                    (entry (nth 0 matches))
                    (email (plist-get entry :user))
                    (pass (plist-get entry :secret)))
              (progn
                (setq bitwarden-user email)
                (if (functionp pass) (funcall pass) pass))
            ""))))
;; Bitwarden:2 ends here

;; [[file:config.org::*Dark mode][Dark mode:1]]
(after! pdf-tools
  (add-hook! 'pdf-view-mode-hook
    (when (memq doom-theme '(modus-vivendi doom-one doom-dark+ doom-vibrant))
      ;; TODO: find a more generic way to detect if we are in a dark theme
      (pdf-view-midnight-minor-mode 1)))

  ;; Color the background, so we can see the PDF page borders
  ;; https://protesilaos.com/emacs/modus-themes#h:ff69dfe1-29c0-447a-915c-b5ff7c5509cd
  (defun +pdf-tools-backdrop ()
    (face-remap-add-relative
     'default
     `(:background ,(if (memq doom-theme '(modus-vivendi modus-operandi))
                        (modus-themes-color 'bg-alt)
                      (doom-color 'bg-alt)))))

  (add-hook 'pdf-tools-enabled-hook #'+pdf-tools-backdrop))

(after! pdf-links
  ;; Tweak for Modus and `pdf-links'
  (when (memq doom-theme '(modus-vivendi modus-operandi))
    ;; https://protesilaos.com/emacs/modus-themes#h:2659d13e-b1a5-416c-9a89-7c3ce3a76574
    (let ((spec (apply #'append
                       (mapcar
                        (lambda (name)
                          (list name
                                (face-attribute 'pdf-links-read-link
                                                name nil 'default)))
                        '(:family :width :weight :slant)))))
      (setq pdf-links-read-link-convert-commands
            `("-density"    "96"
              "-family"     ,(plist-get spec :family)
              "-stretch"    ,(let* ((width (plist-get spec :width))
                                    (name (symbol-name width)))
                               (replace-regexp-in-string "-" ""
                                                         (capitalize name)))
              "-weight"     ,(pcase (plist-get spec :weight)
                               ('ultra-light "Thin")
                               ('extra-light "ExtraLight")
                               ('light       "Light")
                               ('semi-bold   "SemiBold")
                               ('bold        "Bold")
                               ('extra-bold  "ExtraBold")
                               ('ultra-bold  "Black")
                               (_weight      "Normal"))
              "-style"      ,(pcase (plist-get spec :slant)
                               ('italic  "Italic")
                               ('oblique "Oblique")
                               (_slant   "Normal"))
              "-pointsize"  "%P"
              "-undercolor" "%f"
              "-fill"       "%b"
              "-draw"       "text %X,%Y '%c'")))))
;; Dark mode:1 ends here

;; [[file:config.org::*LTDR][LTDR:2]]
(use-package! tldr
  :commands (tldr-update-docs tldr)
  :init
  (setq tldr-enabled-categories '("common" "linux" "osx" "sunos")))
;; LTDR:2 ends here

;; [[file:config.org::*FZF][FZF:2]]
(after! evil
  (evil-define-key 'insert fzf-mode-map (kbd "ESC") #'term-kill-subjob))

(define-minor-mode fzf-mode
  "Minor mode for the FZF buffer"
  :init-value nil
  :lighter " FZF"
  :keymap '(("C-c" . term-kill-subjob)))

(defadvice! doom-fzf--override-start-args-a (original-fn &rest args)
  "Set the FZF minor mode with the fzf buffer."
  :around #'fzf/start
  (message "called with args %S" args)
  (apply original-fn args)

  ;; set the FZF buffer to fzf-mode so we can hook ctrl+c
  (set-buffer "*fzf*")
  (fzf-mode))

(defvar fzf/args
  "-x --print-query -m --tiebreak=index --expect=ctrl-v,ctrl-x,ctrl-t")

(use-package! fzf
  :commands (fzf fzf-projectile fzf-hg fzf-git fzf-git-files fzf-directory fzf-git-grep))
;; FZF:2 ends here

;; [[file:config.org::*Binary files][Binary files:2]]
(defun +buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion (goto-char (point-min))
                    (search-forward (string ?\x00) nil t 1))))

(defun +hexl--buffer-p ()
  (and (+buffer-binary-p)
       ;; Executables are viewed with objdump mode
       (not (+buffer-objdump-p))))

(defun +hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (when (+hexl--buffer-p)
      (hexl-mode))))

(add-to-list 'magic-fallback-mode-alist '(+hexl--buffer-p . +hexl-if-binary) t)
;; Binary files:2 ends here

;; [[file:config.org::*Objdump mode][Objdump mode:1]]
(defun +buffer-objdump-p (&optional buffer file)
  "Can the BUFFER be viewed as a disassembled code with objdump."
  (when-let ((file (or file (buffer-file-name (or buffer (current-buffer))))))
    (and
     (file-exists-p file)
     (not (file-directory-p file))
     (not (zerop (file-attribute-size (file-attributes file))))
     (not (string-match-p
           "file format not recognized"
           (with-temp-buffer
             (shell-command (format "objdump --file-headers %s"
                                    (shell-quote-argument "/home/hacko/Softwares/Kasparov/Kasparov Chessmate/KasparovChess.Stats"))
                            (current-buffer))
             (buffer-string)))))))

(when OBJDUMP-P
  (define-derived-mode objdump-disassemble-mode
    asm-mode "Objdump Mode"
    "Major mode for viewing executable files disassembled using objdump."
    (if (not (+buffer-objdump-p))
        (message "Objdump can not be used with this buffer.")
      (let ((file (buffer-file-name))
            (buffer-read-only nil))
        (erase-buffer)
        (message "Disassembling file \"%s\" using objdump." (file-name-nondirectory file))
        (call-process "objdump" nil (current-buffer) nil "-d" file)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (view-mode)
        (set-visited-file-name nil t))))

  (add-to-list 'magic-fallback-mode-alist '(+buffer-objdump-p . objdump-disassemble-mode) t))
;; Objdump mode:1 ends here

;; [[file:config.org::*Speed Type][Speed Type:2]]
(use-package! speed-type
  :commands (speed-type-text))
;; Speed Type:2 ends here

;; [[file:config.org::*2048 Game][2048 Game:2]]
(use-package! 2048-game
  :commands (2048-game))
;; 2048 Game:2 ends here

;; [[file:config.org::*Snow][Snow:2]]
(use-package! snow
  :commands (snow))
;; Snow:2 ends here

;; [[file:config.org::*=xkcd=][=xkcd=:2]]
(use-package! xkcd
  :commands (xkcd-get xkcd)
  :config
  (setq xkcd-cache-dir (expand-file-name "xkcd/" doom-cache-dir)
        xkcd-cache-latest (expand-file-name "xkcd/latest" doom-cache-dir)))
;; =xkcd=:2 ends here

;; [[file:config.org::*Calendar][Calendar:1]]
(setq calendar-latitude 48.7
      calendar-longitude 2.17
      calendar-location-name "Orsay, FR"
      calendar-time-display-form
      '(24-hours ":" minutes
                 (if time-zone " (") time-zone (if time-zone ")")))
;; Calendar:1 ends here

;; [[file:config.org::*e-Books (=nov=)][e-Books (=nov=):2]]
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat " "
            (propertize (cdr (assoc 'creator nov-metadata))
                        'face 'doom-modeline-project-parent-dir)
            " "
            (cdr (assoc 'title nov-metadata))
            " "
            (propertize (format "%d/%d" (1+ nov-documents-index) (length nov-documents))
                        'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 80
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)

    (add-to-list '+lookup-definition-functions
                 #'+lookup/dictionary-definition)

    (setq-local mode-line-format
                `((:eval
                   (doom-modeline-segment--workspace-name))
                  (:eval
                   (doom-modeline-segment--window-number))
                  (:eval
                   (doom-modeline-segment--nov-info))
                  ,(propertize
                    " %P "
                    'face 'doom-modeline-buffer-minor-mode)
                  ,(propertize
                    " "
                    'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                    'display `((space
                                :align-to
                                (- (+ right right-fringe right-margin)
                                   ,(* (let ((width (doom-modeline--font-width)))
                                         (or (and (= width 1) 1)
                                             (/ width (frame-char-width) 1.0)))
                                       (string-width
                                        (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
                  (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))
;; e-Books (=nov=):2 ends here

;; [[file:config.org::*News feed (=elfeed=)][News feed (=elfeed=):1]]
(setq elfeed-feeds
      '("https://this-week-in-rust.org/rss.xml"
        "https://planet.emacslife.com/atom.xml"
        "https://www.omgubuntu.co.uk/feed"
        "https://itsfoss.com/feed"
        "https://linuxhandbook.com/feed"
        "https://spectrum.ieee.org/rss/robotics/fulltext"
        "https://spectrum.ieee.org/rss/aerospace/fulltext"
        "https://spectrum.ieee.org/rss/computing/fulltext"
        "https://spectrum.ieee.org/rss/blog/automaton/fulltext"
        "https://developers.redhat.com/blog/feed"
        "https://lwn.net/headlines/rss"))
;; News feed (=elfeed=):1 ends here

;; [[file:config.org::*Emacs + NetExtender][Emacs + NetExtender:1]]
(when NETEXTENDER-P
  (defvar +netextender-process-name "netextender")
  (defvar +netextender-buffer-name " *NetExtender*")
  (defvar +netextender-command '("~/.local/bin/netextender"))

  (defun +netextender-start ()
    "Launch a NetExtender VPN session"
    (interactive)
    (unless (get-process +netextender-process-name)
      (if (make-process :name +netextender-process-name
                        :buffer +netextender-buffer-name
                        :command +netextender-command)
          (message "Started NetExtender VPN session")
        (message "Cannot start NetExtender"))))

  (defun +netextender-kill ()
    "Kill the created NetExtender VPN session"
    (interactive)
    (when (get-process +netextender-process-name)
      (if (kill-buffer +netextender-buffer-name)
          (message "Killed NetExtender VPN session")
        (message "Cannot kill NetExtender")))))
;; Emacs + NetExtender:1 ends here

;; [[file:config.org::*Mail client and indexer (=mu= and =mu4e=)][Mail client and indexer (=mu= and =mu4e=):2]]
(after! mu4e
  (require 'mu4e-contrib)
  (require 'mu4e-icalendar)
  (require 'org-agenda)

  ;; Common parameters
  (setq mu4e-update-interval (* 3 60) ;; Every 3 min
        mu4e-index-update-error-warning nil ;; Do not show warning after update
        mu4e-get-mail-command "mbsync -a" ;; Not needed, as +mu4e-backend is 'mbsync by default
        mu4e-main-hide-personal-addresses t ;; No need to display a long list of my own addresses!
        mu4e-attachment-dir (expand-file-name "~/Downloads/mu4e-attachements")
        mu4e-sent-messages-behavior 'sent ;; Save sent messages
        mu4e-context-policy 'pick-first   ;; Start with the first context
        mu4e-compose-context-policy 'ask) ;; Always ask which context to use when composing a new mail

  ;; Use msmtp instead of smtpmail
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-sendmail-envelope-from 'obey-mail-envelope-from
        mail-envelope-from 'header
        mail-personal-alias-file (expand-file-name "mail-aliases.mailrc" doom-user-dir)
        mail-specify-envelope-from t)

  (setq mu4e-headers-fields '((:flags . 6) ;; 3 flags
                              (:account-stripe . 2)
                              (:from-or-to . 25)
                              (:folder . 10)
                              (:recipnum . 2)
                              (:subject . 80)
                              (:human-date . 8))
        +mu4e-min-header-frame-width 142
        mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-time-format "⧖ %H:%M"
        mu4e-search-results-limit 1000
        mu4e-index-cleanup t)

  (defvar +mu4e-header--folder-colors nil)
  (appendq! mu4e-header-info-custom
            '((:folder .
               (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
                (lambda (msg)
                  (+mu4e-colorize-str
                   (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                   '+mu4e-header--folder-colors))))))

  ;; Add a unified inbox shortcut
  (add-to-list
   'mu4e-bookmarks
   '(:name "Unified inbox" :query "maildir:/.*inbox/" :key ?i) t)

  ;; Add shortcut to view yesterday's messages
  (add-to-list
   'mu4e-bookmarks
   '(:name "Yesterday's messages" :query "date:1d..today" :key ?y) t)

  ;; Load a list of my email addresses '+my-addresses', defined as:
  ;; (setq +my-addresses '("user@gmail.com" "user@hotmail.com"))
  (load! "lisp/private/+my-addresses.el")

  (when (bound-and-true-p +my-addresses)
    ;; I like always to add myself in BCC, Lets add a bookmark to show all my BCC mails
    (defun +mu-long-query (query oper arg-list)
      (concat "(" (+str-join (concat " " oper " ") (mapcar (lambda (addr) (format "%s:%s" query addr)) arg-list)) ")"))

    ;; Build a query to match mails send from "me" with "me" in BCC
    (let ((bcc-query (+mu-long-query "bcc" "or" +my-addresses))
          (from-query (+mu-long-query "from" "or" +my-addresses)))
      (add-to-list
       'mu4e-bookmarks
       (list :name "My black copies" :query (format "%s and %s" from-query bcc-query) :key ?k) t)))

  ;; `mu4e-alert' configuration
  ;; Use a nicer icon in alerts
  (setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg")

  (defun +mu4e-alert-helper-name-or-email (msg)
    (let* ((from (car (plist-get msg :from)))
           (name (plist-get from :name)))
      (if (or (null name) (eq name ""))
          (plist-get from :email)
        name)))

  (defun +mu4e-alert-grouped-mail-notif-formatter (mail-group _all-mails)
    (when +mu4e-alert-bell-cmd
      (start-process "mu4e-alert-bell" nil (car +mu4e-alert-bell-cmd) (cdr +mu4e-alert-bell-cmd)))
    (let* ((filtered-mails (+filter
                            (lambda (msg)
                              (not (string-match-p "\\(junk\\|spam\\|trash\\|deleted\\)"
                                                   (downcase (plist-get msg :maildir)))))
                            mail-group))
           (mail-count (length filtered-mails)))
      (list
       :title (format "You have %d unread email%s"
                      mail-count (if (> mail-count 1) "s" ""))
       :body (concat
              "• "
              (+str-join
               "\n• "
               (mapcar
                (lambda (msg)
                  (format "<b>%s</b>: %s"
                          (+mu4e-alert-helper-name-or-email msg)
                          (plist-get msg :subject)))
                filtered-mails))))))

  ;; I use auto-hiding task manager, setting window
  ;; urgency shows the entier task bar (in KDE), which I find annoying.
  (setq mu4e-alert-set-window-urgency nil
        mu4e-alert-grouped-mail-notification-formatter #'+mu4e-alert-grouped-mail-notif-formatter)

  ;; Org-Msg stuff
  ;; org-msg-[signature|greeting-fmt] are separately set for each account
  (setq mail-user-agent 'mu4e-user-agent) ;; Needed by OrgMsg
  (require 'org-msg)
  (setq org-msg-convert-citation t
        org-msg-default-alternatives
        '((new           . (utf-8 html))
          (reply-to-html . (utf-8 html))
          (reply-to-text . (utf-8 html))))

  (map! :map org-msg-edit-mode-map
        :after org-msg
        :n "G" #'org-msg-goto-body)

  (map! :localleader
        :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :desc "Open URL in Brave"   "b" #'browse-url-chrome ;; Brave
        :desc "Open URL in Firefox" "f" #'browse-url-firefox)

  ;; I like to always BCC myself
  (defun +bbc-me ()
    "Add my email to BCC."
    (save-excursion (message-add-header (format "Bcc: %s\n" user-mail-address))))

  (add-hook 'mu4e-compose-mode-hook '+bbc-me)

  ;; Load my accounts
  (load! "lisp/private/+mu4e-accounts.el")

  ;; iCalendar / Org
  (mu4e-icalendar-setup)
  (setq mu4e-icalendar-trash-after-reply nil
        mu4e-icalendar-diary-file "~/Dropbox/Org/diary-invitations.org"
        gnus-icalendar-org-capture-file "~/Dropbox/Org/notes.org"
        gnus-icalendar-org-capture-headline '("Calendar"))

  ;; To enable optional iCalendar->Org sync functionality
  ;; NOTE: both the capture file and the headline(s) inside must already exist
  (gnus-icalendar-org-setup))
;; Mail client and indexer (=mu= and =mu4e=):2 ends here

;; [[file:config.org::*MPD and MPC][MPD and MPC:1]]
;; Not sure if it is required!
(after! mpc
  (setq mpc-host "localhost:6600"))
;; MPD and MPC:1 ends here

;; [[file:config.org::*MPD and MPC][MPD and MPC:2]]
(defun +mpd-daemon-start ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (let ((mpd-daemon-running-p (+mpd-daemon-running-p)))
    (unless mpd-daemon-running-p
      ;; Start the daemon if it is not already running.
      (setq mpd-daemon-running-p (zerop (call-process "systemctl" nil nil nil "--user" "start" "mpd.service"))))
    (cond ((+mpd-daemon-running-p)
           (+mpd-mpc-update)
           (emms-player-mpd-connect)
           (emms-cache-set-from-mpd-all)
           (message "Connected to MPD!"))
          (t
           (warn "An error occured when trying to start Systemd mpd.service.")))))

(defun +mpd-daemon-stop ()
  "Stops playback and kill the MPD daemon."
  (interactive)
  (emms-stop)
  (call-process "systemctl" nil nil nil "--user" "stop" "mpd.service")
  (message "MPD stopped!"))

(defun +mpd-daemon-running-p ()
  "Check if the MPD service is running."
  (zerop (call-process "systemctl" nil nil nil "--user" "is-active" "--quiet" "mpd.service")))

(defun +mpd-mpc-update ()
  "Updates the MPD database synchronously."
  (interactive)
  (if (zerop (call-process "mpc" nil nil nil "update"))
      (message "MPD database updated!")
    (warn "An error occured when trying to update MPD database.")))
;; MPD and MPC:2 ends here

;; [[file:config.org::*EMMS][EMMS:1]]
(after! emms
  ;; EMMS basic configuration
  (require 'emms-setup)

  (when MPD-P
    (require 'emms-player-mpd))

  (emms-all)
  (emms-default-players)

  (setq emms-source-file-default-directory "~/Music/"
        ;; Load cover images
        emms-browser-covers 'emms-browser-cache-thumbnail-async
        emms-seek-seconds 5)

  (if MPD-P
      ;; If using MPD as backend
      (setq emms-player-list '(emms-player-mpd)
            emms-info-functions '(emms-info-mpd)
            emms-player-mpd-server-name "localhost"
            emms-player-mpd-server-port "6600"
            emms-player-mpd-music-directory (expand-file-name "~/Music"))
    ;; Use whatever backend EMMS is using by default (VLC in my machine)
    (setq emms-info-functions '(emms-info-tinytag))) ;; use Tinytag, or '(emms-info-exiftool) for Exiftool

  ;; Keyboard shortcuts
  (global-set-key (kbd "<XF86AudioPrev>")  'emms-previous)
  (global-set-key (kbd "<XF86AudioNext>")  'emms-next)
  (global-set-key (kbd "<XF86AudioPlay>")  'emms-pause)
  (global-set-key (kbd "<XF86AudioPause>") 'emms-pause)
  (global-set-key (kbd "<XF86AudioStop>")  'emms-stop)

  ;; Try to start MPD or connect to it if it is already started.
  (when MPD-P
    (emms-player-set emms-player-mpd 'regex
                     (emms-player-simple-regexp
                      "m3u" "ogg" "flac" "mp3" "wav" "mod" "au" "aiff"))
    (add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)
    (+mpd-daemon-start))

  ;; Activate EMMS in mode line
  (emms-mode-line 1)

  ;; More descriptive track lines in playlists
  ;; From: https://www.emacswiki.org/emacs/EMMS#h5o-15
  (defun +better-emms-track-description (track)
    "Return a somewhat nice track description."
    (let ((artist (emms-track-get track 'info-artist))
          (album (emms-track-get track 'info-album))
          (tracknumber (emms-track-get track 'info-tracknumber))
          (title (emms-track-get track 'info-title)))
      (cond
       ((or artist title)
        (concat
         (if (> (length artist) 0) artist "Unknown artist") ": "
         (if (> (length album) 0) album "Unknown album") " - "
         (if (> (length tracknumber) 0) (format "%02d. " (string-to-number tracknumber)) "")
         (if (> (length title) 0) title "Unknown title")))
       (t
        (emms-track-simple-description track)))))

  (setq emms-track-description-function '+better-emms-track-description)

  ;; Manage notifications, inspired by:
  ;; https://www.emacswiki.org/emacs/EMMS#h5o-9
  ;; https://www.emacswiki.org/emacs/EMMS#h5o-11
  (cond
   ;; Choose D-Bus to disseminate messages, if available.
   ((and (require 'dbus nil t) (dbus-ping :session "org.freedesktop.Notifications"))
    (setq +emms-notifier-function '+notify-via-freedesktop-notifications)
    (require 'notifications))
   ;; Try to make use of KNotify if D-Bus isn't present.
   ((and window-system (executable-find "kdialog"))
    (setq +emms-notifier-function '+notify-via-kdialog))
   ;; Use the message system otherwise
   (t (setq +emms-notifier-function '+notify-via-messages)))

  (setq +emms-notification-icon "/usr/share/icons/Papirus/64x64/apps/enjoy-music-player.svg")

  (defun +notify-via-kdialog (title msg icon)
    "Send notification with TITLE, MSG, and ICON via `KDialog'."
    (call-process "kdialog"
                  nil nil nil
                  "--title" title
                  "--passivepopup" msg "5"
                  "--icon" icon))

  (defun +notify-via-freedesktop-notifications (title msg icon)
    "Send notification with TITLE, MSG, and ICON via `D-Bus'."
    (notifications-notify
     :title title
     :body msg
     :app-icon icon
     :urgency 'low))

  (defun +notify-via-messages (title msg icon)
    "Send notification with TITLE, MSG to message. ICON is ignored."
    (message "%s %s" title msg))

  (add-hook 'emms-player-started-hook
            (lambda () (funcall +emms-notifier-function
                                "EMMS is now playing:"
                                (emms-track-description (emms-playlist-current-selected-track))
                                +emms-notification-icon))))
;; EMMS:1 ends here

;; [[file:config.org::*EMPV][EMPV:2]]
(use-package! empv
  :when MPV-P
  :init
  (map! :leader :prefix ("l m")
        (:prefix ("v" . "empv")
         :desc "Play"                  "p" #'empv-play
         :desc "Seach Youtube"         "y" #'consult-empv-youtube
         :desc "Play radio"            "r" #'empv-play-radio
         :desc "Save current playlist" "s" #'+empv-save-playtlist-to-file))
  :config
  ;; See https://docs.invidious.io/instances/
  (setq empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1"
        empv-audio-dir "~/Music"
        empv-video-dir "~/Videos"
        empv-max-directory-search-depth 6
        empv-radio-log-file (expand-file-name "logged-radio-songs.org" org-directory)
        empv-audio-file-extensions '("webm" "mp3" "ogg" "wav" "m4a" "flac" "aac" "opus")
        ;; Links from https://www.radio-browser.info
        empv-radio-channels
        '(("El-Bahdja FM" . "http://webradio.tda.dz:8001/ElBahdja_64K.mp3")
          ("El-Chaabia" . "https://radio-dzair.net/proxy/chaabia?mp=/stream")
          ("Quran Radio" . "http://stream.radiojar.com/0tpy1h0kxtzuv")
          ("Algeria International" . "https://webradio.tda.dz/Internationale_64K.mp3")
          ("JOW Radio" . "https://str0.creacast.com/jowradio")
          ("Europe1" . "http://ais-live.cloud-services.paris:8000/europe1.mp3")
          ("France Iter" . "http://direct.franceinter.fr/live/franceinter-hifi.aac")
          ("France Info" . "http://direct.franceinfo.fr/live/franceinfo-midfi.mp3")
          ("France Culture" . "http://icecast.radiofrance.fr/franceculture-hifi.aac")
          ("France Musique" . "http://icecast.radiofrance.fr/francemusique-hifi.aac")
          ("FIP" . "http://icecast.radiofrance.fr/fip-hifi.aac")
          ("Beur FM" . "http://broadcast.infomaniak.ch/beurfm-high.aac")
          ("Skyrock" . "http://icecast.skyrock.net/s/natio_mp3_128k")))

  (empv-playlist-loop-on)

  ;; Hacky palylist management (only supports saving playlist,
  ;; loading a playlist can be achieved using `empv-play-file')

  (defvar +empv-playlist-dir empv-audio-dir)

  (defun +empv--playlist-apply (fn &rest args)
    (empv--let-properties '(playlist)
     (let ((files (mapcar (lambda (it) (alist-get 'filename it)) .playlist)))
       (apply fn files args))))

  (defun +empv-save-playtlist-to-file (path)
    (interactive "FSave playlist to: ")
    (+empv--playlist-apply #'+empv--save-playlist-to-file path))

  (defun +empv--save-playlist-to-file (playlist &optional filename)
    (with-temp-buffer
      (insert (+str-join "\n" playlist))
      (let* ((last-pl (file-name-sans-extension (car (last (directory-files +empv-playlist-dir nil "empv-playlist-\\([[:digit:]]+\\)\\.m3u")))))
             (num (if (null last-pl) 0 (1+ (string-to-number (car (last (+str-split last-pl "-")))))))
             (filename (or filename (expand-file-name (format "empv-playlist-%d.m3u" num) +empv-playlist-dir))))
        ;; (if (file-exists-p filename)
        ;;     (append-to-file (point-min) (point-max) filename)
        (write-file filename))))

  (defun +empv--dl-playlist (playlist &optional dist)
    (let ((default-directory
            (or dist
                (let ((d (expand-file-name "empv-downloads" empv-audio-dir)))
                  (unless (file-directory-p d) (mkdir d t)) d)))
          (vids (+filter
                 'identity ;; Filter nils
                 (mapcar
                  (lambda (item)
                    (when-let
                        ((vid (when (string-match
                                     (rx (seq "watch?v=" (group-n 1 (one-or-more (or alnum "_" "-")))))
                                     item)
                                (match-string 1 item))))
                      vid))
                  playlist)))
          (proc-name "empv-yt-dlp"))
      (unless (zerop (length vids))
        (message "Downloading %d songs to %s" (length vids) default-directory)
        (when (get-process proc-name)
          (kill-process proc-name))
        (make-process :name proc-name
                      :buffer (format "*%s*" proc-name)
                      :command (append
                                (list
                                 (executable-find "yt-dlp")
                                 "--no-abort-on-error"
                                 "--no-colors"
                                 "--extract-audio"
                                 "--no-progress"
                                 "-f" "bestaudio")
                                vids)
                      :sentinel (lambda (prc event)
                                  (when (string= event "finished\n")
                                    (message "Finished downloading playlist files!")))))))

  (defun +empv-download-playtlist-files (&optional path)
    (interactive "DSave download playlist files to: ")
    (+empv--playlist-apply #'+empv--dl-playlist path)))
;; EMPV:2 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
(map! :leader :prefix ("l" . "custom")
      (:when (modulep! :app emms)
       :prefix ("m" . "media")
       :desc "Playlist go"                 "g" #'emms-playlist-mode-go
       :desc "Add playlist"                "D" #'emms-add-playlist
       :desc "Toggle random playlist"      "r" #'emms-toggle-random-playlist
       :desc "Add directory"               "d" #'emms-add-directory
       :desc "Add file"                    "f" #'emms-add-file
       :desc "Smart browse"                "b" #'emms-smart-browse
       :desc "Play/Pause"                  "p" #'emms-pause
       :desc "Start"                       "S" #'emms-start
       :desc "Stop"                        "s" #'emms-stop))
;; Keybindings:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:2]]
(map! :leader :prefix ("l m")
      (:when (and (modulep! :app emms) MPD-P)
       :prefix ("m" . "mpd/mpc")
       :desc "Start daemon"              "s" #'+mpd-daemon-start
       :desc "Stop daemon"               "k" #'+mpd-daemon-stop
       :desc "EMMS player (MPD update)"  "R" #'emms-player-mpd-update-all-reset-cache
       :desc "Update database"           "u" #'+mpd-mpc-update))
;; Keybindings:2 ends here

;; [[file:config.org::*Cycle song information in mode line][Cycle song information in mode line:2]]
(use-package! emms-mode-line-cycle
  :after emms
  :config
  (setq emms-mode-line-cycle-max-width 15
        emms-mode-line-cycle-additional-space-num 4
        emms-mode-line-cycle-any-width-p nil
        emms-mode-line-cycle-velocity 4)

  ;; Some music files do not have metadata, by default, the track title
  ;; will be the full file path, so, if I detect what seems to be an absolute
  ;; path, I trim the directory part and get only the file name.
  (setq emms-mode-line-cycle-current-title-function
        (lambda ()
          (let ((name (emms-track-description (emms-playlist-current-selected-track))))
            (if (file-name-absolute-p name) (file-name-base name) name))))

  ;; Mode line formatting settings
  ;; This format complements the 'emms-mode-line-format' one.
  (setq emms-mode-line-format " ⟨⏵ %s⟩"  ;; 𝅘𝅥𝅮 ⏵ ⏸
        ;; To hide the playing time without stopping the cycling.
        emms-playing-time-display-format "")

  (defun +emms-mode-line-toggle-format-hook ()
    "Toggle the 'emms-mode-line-fotmat' string, when playing or paused."
    (setq emms-mode-line-format (concat " ⟨" (if emms-player-paused-p "⏸" "⏵") " %s⟩"))
    ;; Force a sync to get the right song name over MPD in mode line
    (when MPD-P (emms-player-mpd-sync-from-mpd))
    ;; Trigger a forced update of mode line (useful when pausing)
    (emms-mode-line-alter-mode-line))

      ;; Hook the function to the 'emms-player-paused-hook'
  (add-hook 'emms-player-paused-hook '+emms-mode-line-toggle-format-hook)

  (emms-mode-line-cycle 1))
;; Cycle song information in mode line:2 ends here

;; [[file:config.org::*Maxima][Maxima:2]]
(use-package! maxima
  :when MAXIMA-P
  :commands (maxima-mode maxima-inferior-mode maxima)
  :init
  (require 'straight) ;; to use `straight-build-dir' and `straight-base-dir'
  (setq maxima-font-lock-keywords-directory ;; a workaround to undo the straight workaround!
        (expand-file-name (format "straight/%s/maxima/keywords" straight-build-dir) straight-base-dir))

  ;; The `maxima-hook-function' setup `company-maxima'.
  (add-hook 'maxima-mode-hook #'maxima-hook-function)
  (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode)))
;; Maxima:2 ends here

;; [[file:config.org::*IMaxima][IMaxima:2]]
(use-package! imaxima
  :when MAXIMA-P
  :commands (imaxima imath-mode)
  :init
  (setq imaxima-use-maxima-mode-flag nil ;; otherwise, it don't render equations with LaTeX.
        imaxima-scale-factor 2.0)

  ;; Hook the `maxima-inferior-mode' to get Company completion.
  (add-hook 'imaxima-startup-hook #'maxima-inferior-mode))
;; IMaxima:2 ends here

;; [[file:config.org::*FriCAS][FriCAS:1]]
(use-package! fricas
  :when FRICAS-P
  :load-path "/usr/lib/fricas/emacs"
  :commands (fricas-mode fricas-eval fricas))
;; FriCAS:1 ends here

;; [[file:config.org::*CSV rainbow][CSV rainbow:1]]
(after! csv-mode
  ;; TODO: Need to fix the case of two commas, example "a,b,,c,d"
  (require 'cl-lib)
  (require 'color)

  (map! :localleader
        :map csv-mode-map
        "R" #'+csv-rainbow)

  (defun +csv-rainbow (&optional separator)
    (interactive (list (when current-prefix-arg (read-char "Separator: "))))
    (font-lock-mode 1)
    (let* ((separator (or separator ?\,))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                            collect (apply #'color-rgb-to-hex
                                           (color-hsl-to-rgb i 0.3 0.5)))))
      (cl-loop for i from 2 to n by 2
               for c in colors
               for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
               do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c))))))))))

;; provide CSV mode setup
;; (add-hook 'csv-mode-hook (lambda () (+csv-rainbow)))
;; CSV rainbow:1 ends here

;; [[file:config.org::*Vimrc][Vimrc:2]]
(use-package! vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")
;; Vimrc:2 ends here

;; [[file:config.org::*Python IDE][Python IDE:2]]
(use-package! elpy
  :hook ((elpy-mode . flycheck-mode)
         (elpy-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             '((elpy-company-backend :with company-yasnippet))))))
  :config
  ;;:init
  (elpy-enable))
;; Python IDE:2 ends here

;; [[file:config.org::*GNU Octave][GNU Octave:1]]
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; GNU Octave:1 ends here

;; [[file:config.org::*GNU Octave][GNU Octave:2]]
(defun +octave-eval-last-sexp ()
  "Evaluate Octave sexp before point and print value into current buffer."
  (interactive)
  (inferior-octave t)
  (let ((print-escape-newlines nil)
        (opoint (point)))
    (prin1
     (save-excursion
       (forward-sexp -1)
       (inferior-octave-send-list-and-digest
        (list (concat (buffer-substring-no-properties (point) opoint)
                      "\n")))
       (mapconcat 'identity inferior-octave-output-list "\n")))))

(defun +eros-octave-eval-last-sexp ()
  "Wrapper for `+octave-eval-last-sexp' that overlays results."
  (interactive)
  (eros--eval-overlay
   (octave-eval-last-sexp)
   (point)))

(map! :localleader
      :map (octave-mode-map)
      (:prefix ("e" . "eval")
       :desc "Eval and print last sexp" "e" #'+eros-octave-eval-last-sexp))
;; GNU Octave:2 ends here

;; [[file:config.org::*Extensions][Extensions:1]]
(add-to-list 'auto-mode-alist '("\\.rviz\\'"   . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'"   . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro\\'"  . xml-mode))
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Use gdb-script-mode for msg and srv files
(add-to-list 'auto-mode-alist '("\\.msg\\'"    . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.srv\\'"    . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.action\\'" . gdb-script-mode))
;; Extensions:1 ends here

;; [[file:config.org::*ROS bags][ROS bags:1]]
(when ROSBAG-P
  (define-derived-mode rosbag-view-mode
    fundamental-mode "Rosbag view mode"
    "Major mode for viewing ROS bag files."
    (let ((f (buffer-file-name)))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (message "Calling rosbag info")
        (call-process "rosbag" nil (current-buffer) nil
                      "info" f)
        (set-buffer-modified-p nil))
      (view-mode)
      (set-visited-file-name nil t)))

  ;; rosbag view mode
  (add-to-list 'auto-mode-alist '("\\.bag$" . rosbag-view-mode)))
;; ROS bags:1 ends here

;; [[file:config.org::*=ros.el=][=ros.el=:2]]
(use-package! ros
  :init
  (map! :leader
        :prefix ("l" . "custom")
        :desc "Hydra ROS" "r" #'hydra-ros-main/body)
  :commands (hydra-ros-main/body ros-set-workspace)
  :config
  (setq ros-workspaces
        (list (ros-dump-workspace
               :tramp-prefix (format "/docker:%s@%s:" "ros" "ros-machine")
               :workspace "~/ros_ws"
               :extends '("/opt/ros/noetic/"))
              (ros-dump-workspace
               :tramp-prefix (format "/ssh:%s@%s:" "swd_sk" "172.16.96.42")
               :workspace "~/ros_ws"
               :extends '("/opt/ros/noetic/"))
              (ros-dump-workspace
               :tramp-prefix (format "/ssh:%s@%s:" "swd_sk" "172.16.96.42")
               :workspace "~/ros2_ws"
               :extends '("/opt/ros/foxy/")))))
;; =ros.el=:2 ends here

;; [[file:config.org::*Scheme][Scheme:1]]
(after! geiser
  (setq geiser-default-implementation 'guile
        geiser-chez-binary "chez-scheme")) ;; default is "scheme"
;; Scheme:1 ends here

;; [[file:config.org::*Embed.el][Embed.el:2]]
(use-package! embed
  :commands (embed-openocd-start
             embed-openocd-stop
             embed-openocd-gdb
             embed-openocd-flash)

  :init
  (map! :leader :prefix ("l" . "custom")
        (:when (modulep! :tools debugger +lsp)
         :prefix ("e" . "embedded")
         :desc "Start OpenOCD"    "o" #'embed-openocd-start
         :desc "Stop OpenOCD"     "O" #'embed-openocd-stop
         :desc "OpenOCD GDB"      "g" #'embed-openocd-gdb
         :desc "OpenOCD flash"    "f" #'embed-openocd-flash)))
;; Embed.el:2 ends here

;; [[file:config.org::*Bitbake (Yocto)][Bitbake (Yocto):2]]
(use-package! bitbake-modes
  :commands (wks-mode
             mmm-mode
             bb-sh-mode
             bb-scc-mode
             bitbake-mode
             conf-bitbake-mode
             bitbake-task-log-mode))
;; Bitbake (Yocto):2 ends here

;; [[file:config.org::*Magit][Magit:1]]
(after! code-review
  (setq code-review-auth-login-marker 'forge))
;; Magit:1 ends here

;; [[file:config.org::*Granular diff-highlights for /all/ hunks][Granular diff-highlights for /all/ hunks:1]]
(after! magit
  ;; Disable if it causes performance issues
  (setq magit-diff-refine-hunk t))
;; Granular diff-highlights for /all/ hunks:1 ends here

;; [[file:config.org::*Gravatars][Gravatars:1]]
(after! magit
  ;; Show gravatars
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))
;; Gravatars:1 ends here

;; [[file:config.org::*WIP Company for commit messages][WIP Company for commit messages:2]]
(use-package! company-gitcommit
  :init
  (add-hook
   'git-commit-setup-hook
   (lambda ()
     (let ((backends (car company-backends)))
       (setq company-backend
             (if (listp backends)
                 (cons (append backends 'company-gitcommit) (car company-backends))
               (append company-backends (list 'company-gitcommit))))))))
;; WIP Company for commit messages:2 ends here

;; [[file:config.org::*Pretty graph][Pretty graph:2]]
(use-package! magit-pretty-graph
  :after magit
  :init
  (setq magit-pg-command
        (concat "git --no-pager log"
                " --topo-order --decorate=full"
                " --pretty=format:\"%H%x00%P%x00%an%x00%ar%x00%s%x00%d\""
                " -n 2000")) ;; Increase the default 100 limit

  (map! :localleader
        :map (magit-mode-map)
        :desc "Magit pretty graph" "p" (lambda () (interactive) (magit-pg-repo (magit-toplevel)))))
;; Pretty graph:2 ends here

;; [[file:config.org::*Repo][Repo:2]]
(use-package! repo
  :when REPO-P
  :commands repo-status)
;; Repo:2 ends here

;; [[file:config.org::*Blamer][Blamer:2]]
(use-package! blamer
  :commands (blamer-mode)
  ;; :hook ((prog-mode . blamer-mode))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 60)
  (blamer-prettify-time-p t)
  (blamer-entire-formatter "    %s")
  (blamer-author-formatter " %s ")
  (blamer-datetime-formatter "[%s], ")
  (blamer-commit-formatter "“%s”")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 125
                   :italic t)))
  :config
  (when (modulep! :ui zen) ;; Disable in zen (writeroom) mode
    (add-hook 'writeroom-mode-enable-hook
              (when (bound-and-true-p blamer-mode)
                (setq +blamer-mode--was-active-p t)
                (blamer-mode -1)))
    (add-hook 'writeroom-mode-disable-hook
              (when (bound-and-true-p +blamer-mode--was-active-p)
                (blamer-mode 1)))))
;; Blamer:2 ends here

;; [[file:config.org::*Assembly][Assembly:2]]
(use-package! nasm-mode
  :mode "\\.[n]*\\(asm\\|s\\)\\'")

;; Get Haxor VM from https://github.com/krzysztof-magosa/haxor
(use-package! haxor-mode
  :mode "\\.hax\\'")

(use-package! mips-mode
  :mode "\\.mips\\'")

(use-package! riscv-mode
  :mode "\\.riscv\\'")

(use-package! x86-lookup
  :commands (x86-lookup)
  :config
  (when (modulep! :tools pdf)
    (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools))
  ;; Get manual from https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (setq x86-lookup-pdf (expand-file-name "x86-lookup/325383-sdm-vol-2abcd.pdf" doom-data-dir)))
;; Assembly:2 ends here

;; [[file:config.org::*Disaster][Disaster:2]]
(use-package! disaster
  :commands (disaster)
  :init
  (setq disaster-assembly-mode 'nasm-mode)

  (map! :localleader
        :map (c++-mode-map c-mode-map fortran-mode)
        :desc "Disaster" "d" #'disaster))
;; Disaster:2 ends here

;; [[file:config.org::*Devdocs][Devdocs:2]]
(use-package! devdocs
  :commands (devdocs-lookup devdocs-install)
  :config
  (setq devdocs-data-dir (expand-file-name "devdocs" doom-data-dir)))
;; Devdocs:2 ends here

;; [[file:config.org::*Systemd][Systemd:2]]
(use-package! journalctl-mode
  :commands (journalctl
             journalctl-boot
             journalctl-unit
             journalctl-user-unit)
  :init
  (map! :map journalctl-mode-map
        :nv "J" #'journalctl-next-chunk
        :nv "K" #'journalctl-previous-chunk))
;; Systemd:2 ends here

;; [[file:config.org::*PKGBUILD][PKGBUILD:2]]
(use-package! pkgbuild-mode
  :commands (pkgbuild-mode)
  :mode "/PKGBUILD$")
;; PKGBUILD:2 ends here

;; [[file:config.org::*Franca IDL][Franca IDL:2]]
(use-package! franca-idl
  :commands franca-idl-mode)
;; Franca IDL:2 ends here

;; [[file:config.org::*LaTeX][LaTeX:2]]
(use-package! aas
  :commands aas-mode)
;; LaTeX:2 ends here

;; [[file:config.org::*Flycheck + Projectile][Flycheck + Projectile:2]]
(use-package! flycheck-projectile
  :commands flycheck-projectile-list-errors)
;; Flycheck + Projectile:2 ends here

;; [[file:config.org::*Graphviz][Graphviz:2]]
(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gv\\'")
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes) 'graphviz-dot)))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)
;; Graphviz:2 ends here

;; [[file:config.org::*LSP][LSP:2]]
(use-package! lsp-dot)
;; LSP:2 ends here

;; [[file:config.org::*Mermaid][Mermaid:2]]
(use-package! mermaid-mode
  :commands mermaid-mode
  :mode "\\.mmd\\'")

(use-package! ob-mermaid
  :after org
  :init
  (after! org
    (add-to-list 'org-babel-load-languages '(mermaid . t))))
;; Mermaid:2 ends here

;; [[file:config.org::*The V Programming Language][The V Programming Language:2]]
(use-package! v-mode
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode)
  :config
  (map! :localleader
        :map (v-mode-map)
        :desc "v-format-buffer" "f" #'v-format-buffer
        :desc "v-menu" "m" #'v-menu))
;; The V Programming Language:2 ends here

;; [[file:config.org::*Inspector][Inspector:2]]
(use-package! inspector
  :commands (inspect-expression inspect-last-sexp))
;; Inspector:2 ends here

(after! org
  (setq org-directory "~/Dropbox/Org/" ; let's put files here
        org-use-property-inheritance t ; it's convenient to have properties inherited
        org-log-done 'time             ; having the time an item is done sounds convenient
        org-list-allow-alphabetical t  ; have a. A. a) A) list bullets
        org-export-in-background nil   ; run export processes in external emacs process
        org-export-async-debug t
        org-tags-column 0
        org-catch-invisible-edits 'smart ;; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{} ;; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        org-pretty-entities-include-sub-superscripts nil
        org-auto-align-tags nil
        org-special-ctrl-a/e t
        org-startup-indented t ;; Enable 'org-indent-mode' by default, override with '+#startup: noindent' for big files
        org-insert-heading-respect-content t)
  (setq org-babel-default-header-args
        '((:session  . "none")
          (:results  . "replace")
          (:exports  . "code")
          (:cache    . "no")
          (:noweb    . "no")
          (:hlines   . "no")
          (:tangle   . "no")
          (:comments . "link")))
  ;; stolen from https://github.com/yohan-pereira/.emacs#babel-config
  (defun +org-confirm-babel-evaluate (lang body)
    (not (string= lang "scheme"))) ;; Don't ask for scheme
  
  (setq org-confirm-babel-evaluate #'+org-confirm-babel-evaluate)
  (map! :map evil-org-mode-map
        :after evil-org
        :n "g <up>" #'org-backward-heading-same-level
        :n "g <down>" #'org-forward-heading-same-level
        :n "g <left>" #'org-up-element
        :n "g <right>" #'org-down-element)
  (setq org-todo-keywords
        '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  
  (setq org-todo-keyword-faces
        '(("IDEA" . (:foreground "goldenrod" :weight bold))
          ("NEXT" . (:foreground "IndianRed1" :weight bold))
          ("STRT" . (:foreground "OrangeRed" :weight bold))
          ("WAIT" . (:foreground "coral" :weight bold))
          ("KILL" . (:foreground "DarkGreen" :weight bold))
          ("PROJ" . (:foreground "LimeGreen" :weight bold))
          ("HOLD" . (:foreground "orange" :weight bold))))
  
  (defun +log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  
  (add-hook 'org-after-todo-state-change-hook #'+log-todo-next-creation-date)
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
  
  (setq org-agenda-files
        (list (expand-file-name "inbox.org" org-directory)
              (expand-file-name "agenda.org" org-directory)
              (expand-file-name "gcal-agenda.org" org-directory)
              (expand-file-name "notes.org" org-directory)
              (expand-file-name "projects.org" org-directory)
              (expand-file-name "archive.org" org-directory)))
  ;; Agenda styling
  (setq org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
  (use-package! org-super-agenda
    :defer t
    :config
    (org-super-agenda-mode)
    :init
    (setq org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-include-deadlines t
          org-agenda-block-separator nil
          org-agenda-tags-column 100 ;; from testing this seems to be a good value
          org-agenda-compact-blocks t)
  
    (setq org-agenda-custom-commands
          '(("o" "Overview"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                              :time-grid t
                              :date today
                              :todo "TODAY"
                              :scheduled today
                              :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '((:name "Next to do" :todo "NEXT" :order 1)
                              (:name "Important" :tag "Important" :priority "A" :order 6)
                              (:name "Due Today" :deadline today :order 2)
                              (:name "Due Soon" :deadline future :order 8)
                              (:name "Overdue" :deadline past :face error :order 7)
                              (:name "Assignments" :tag "Assignment" :order 10)
                              (:name "Issues" :tag "Issue" :order 12)
                              (:name "Emacs" :tag "Emacs" :order 13)
                              (:name "Projects" :tag "Project" :order 14)
                              (:name "Research" :tag "Research" :order 15)
                              (:name "To read" :tag "Read" :order 30)
                              (:name "Waiting" :todo "WAIT" :order 20)
                              (:name "University" :tag "Univ" :order 32)
                              (:name "Trivial" :priority<= "E" :tag ("Trivial" "Unimportant") :todo ("SOMEDAY") :order 90)
                              (:discard (:tag ("Chore" "Routine" "Daily"))))))))))))
  (after! org-gcal
    (load! "lisp/private/+org-gcal.el"))
  (use-package! caldav
    :commands (org-caldav-sync))
  (setq +org-capture-emails-file (expand-file-name "inbox.org" org-directory)
        +org-capture-todo-file (expand-file-name "inbox.org" org-directory)
        +org-capture-projects-file (expand-file-name "projects.org" org-directory))
  (use-package! doct
    :commands (doct))
  (after! org-capture
    (defun org-capture-select-template-prettier (&optional keys)
      "Select a capture template, in a prettier way than default
    Lisp programs can force the template by setting KEYS to a string."
      (let ((org-capture-templates
             (or (org-contextualize-keys
                  (org-capture-upgrade-templates org-capture-templates)
                  org-capture-templates-contexts)
                 '(("t" "Task" entry (file+headline "" "Tasks")
                    "* TODO %?\n  %u\n  %a")))))
        (if keys
            (or (assoc keys org-capture-templates)
                (error "No capture template referred to by \"%s\" keys" keys))
          (org-mks org-capture-templates
                   "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                   "Template key: "
                   `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
    (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
    
    (defun org-mks-pretty (table title &optional prompt specials)
      "Select a member of an alist with multiple keys. Prettified.
    
    TABLE is the alist which should contain entries where the car is a string.
    There should be two types of entries.
    
    1. prefix descriptions like (\"a\" \"Description\")
       This indicates that `a' is a prefix key for multi-letter selection, and
       that there are entries following with keys like \"ab\", \"ax\"…
    
    2. Select-able members must have more than two elements, with the first
       being the string of keys that lead to selecting it, and the second a
       short description string of the item.
    
    The command will then make a temporary buffer listing all entries
    that can be selected with a single key, and all the single key
    prefixes.  When you press the key for a single-letter entry, it is selected.
    When you press a prefix key, the commands (and maybe further prefixes)
    under this key will be shown and offered for selection.
    
    TITLE will be placed over the selection in the temporary buffer,
    PROMPT will be used when prompting for a key.  SPECIALS is an
    alist with (\"key\" \"description\") entries.  When one of these
    is selected, only the bare key is returned."
      (save-window-excursion
        (let ((inhibit-quit t)
              (buffer (org-switch-to-buffer-other-window "*Org Select*"))
              (prompt (or prompt "Select: "))
              case-fold-search
              current)
          (unwind-protect
              (catch 'exit
                (while t
                  (setq-local evil-normal-state-cursor (list nil))
                  (erase-buffer)
                  (insert title "\n\n")
                  (let ((des-keys nil)
                        (allowed-keys '("\C-g"))
                        (tab-alternatives '("\s" "\t" "\r"))
                        (cursor-type nil))
                    ;; Populate allowed keys and descriptions keys
                    ;; available with CURRENT selector.
                    (let ((re (format "\\`%s\\(.\\)\\'"
                                      (if current (regexp-quote current) "")))
                          (prefix (if current (concat current " ") "")))
                      (dolist (entry table)
                        (pcase entry
                          ;; Description.
                          (`(,(and key (pred (string-match re))) ,desc)
                           (let ((k (match-string 1 key)))
                             (push k des-keys)
                             ;; Keys ending in tab, space or RET are equivalent.
                             (if (member k tab-alternatives)
                                 (push "\t" allowed-keys)
                               (push k allowed-keys))
                             (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                          ;; Usable entry.
                          (`(,(and key (pred (string-match re))) ,desc . ,_)
                           (let ((k (match-string 1 key)))
                             (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                             (push k allowed-keys)))
                          (_ nil))))
                    ;; Insert special entries, if any.
                    (when specials
                      (insert "─────────────────────────\n")
                      (pcase-dolist (`(,key ,description) specials)
                        (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                        (push key allowed-keys)))
                    ;; Display UI and let user select an entry or
                    ;; a sublevel prefix.
                    (goto-char (point-min))
                    (unless (pos-visible-in-window-p (point-max))
                      (org-fit-window-to-buffer))
                    (let ((pressed (org--mks-read-key allowed-keys
                                                      prompt
                                                      (not (pos-visible-in-window-p (1- (point-max)))))))
                      (setq current (concat current pressed))
                      (cond
                       ((equal pressed "\C-g") (user-error "Abort"))
                       ;; Selection is a prefix: open a new menu.
                       ((member pressed des-keys))
                       ;; Selection matches an association: return it.
                       ((let ((entry (assoc current table)))
                          (and entry (throw 'exit entry))))
                       ;; Selection matches a special entry: return the
                       ;; selection prefix.
                       ((assoc current specials) (throw 'exit current))
                       (t (error "No entry available")))))))
            (when buffer (kill-buffer buffer))))))
    (advice-add 'org-mks :override #'org-mks-pretty)
  
    (defun +doct-icon-declaration-to-icon (declaration)
      "Convert :icon declaration to icon"
      (let ((name (pop declaration))
            (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
            (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
            (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
        (apply set `(,name :face ,face :v-adjust ,v-adjust))))
  
    (defun +doct-iconify-capture-templates (groups)
      "Add declaration's :icon to each template group in GROUPS."
      (let ((templates (doct-flatten-lists-in groups)))
        (setq doct-templates
              (mapcar (lambda (template)
                        (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                    (spec (plist-get (plist-get props :doct) :icon)))
                          (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                         "\t"
                                                         (nth 1 template))))
                        template)
                      templates))))
  
    (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))
  
    (defun set-org-capture-templates ()
      (setq org-capture-templates
            (doct `(("Personal todo" :keys "t"
                     :icon ("checklist" :set "octicon" :color "green")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* TODO %?"
                                "%i %a"))
                    ("Personal note" :keys "n"
                     :icon ("sticky-note-o" :set "faicon" :color "green")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* %?"
                                "%i %a"))
                    ("Email" :keys "e"
                     :icon ("envelope" :set "faicon" :color "blue")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                                "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                                "about %^{topic}"
                                "%U %i %a"))
                    ("Interesting" :keys "i"
                     :icon ("eye" :set "faicon" :color "lcyan")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Interesting"
                     :type entry
                     :template ("* [ ] %{desc}%? :%{i-type}:"
                                "%i %a")
                     :children (("Webpage" :keys "w"
                                 :icon ("globe" :set "faicon" :color "green")
                                 :desc "%(org-cliplink-capture) "
                                 :i-type "read:web")
                                ("Article" :keys "a"
                                 :icon ("file-text" :set "octicon" :color "yellow")
                                 :desc ""
                                 :i-type "read:reaserch")
                                ("Information" :keys "i"
                                 :icon ("info-circle" :set "faicon" :color "blue")
                                 :desc ""
                                 :i-type "read:info")
                                ("Idea" :keys "I"
                                 :icon ("bubble_chart" :set "material" :color "silver")
                                 :desc ""
                                 :i-type "idea")))
                    ("Tasks" :keys "k"
                     :icon ("inbox" :set "octicon" :color "yellow")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Tasks"
                     :type entry
                     :template ("* TODO %? %^G%{extra}"
                                "%i %a")
                     :children (("General Task" :keys "k"
                                 :icon ("inbox" :set "octicon" :color "yellow")
                                 :extra "")
  
                                ("Task with deadline" :keys "d"
                                 :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                 :extra "\nDEADLINE: %^{Deadline:}t")
  
                                ("Scheduled Task" :keys "s"
                                 :icon ("calendar" :set "octicon" :color "orange")
                                 :extra "\nSCHEDULED: %^{Start time:}t")))
                    ("Project" :keys "p"
                     :icon ("repo" :set "octicon" :color "silver")
                     :prepend t
                     :type entry
                     :headline "Inbox"
                     :template ("* %{time-or-todo} %?"
                                "%i"
                                "%a")
                     :file ""
                     :custom (:time-or-todo "")
                     :children (("Project-local todo" :keys "t"
                                 :icon ("checklist" :set "octicon" :color "green")
                                 :time-or-todo "TODO"
                                 :file +org-capture-project-todo-file)
                                ("Project-local note" :keys "n"
                                 :icon ("sticky-note" :set "faicon" :color "yellow")
                                 :time-or-todo "%U"
                                 :file +org-capture-project-notes-file)
                                ("Project-local changelog" :keys "c"
                                 :icon ("list" :set "faicon" :color "blue")
                                 :time-or-todo "%U"
                                 :heading "Unreleased"
                                 :file +org-capture-project-changelog-file)))
                    ("\tCentralised project templates"
                     :keys "o"
                     :type entry
                     :prepend t
                     :template ("* %{time-or-todo} %?"
                                "%i"
                                "%a")
                     :children (("Project todo"
                                 :keys "t"
                                 :prepend nil
                                 :time-or-todo "TODO"
                                 :heading "Tasks"
                                 :file +org-capture-central-project-todo-file)
                                ("Project note"
                                 :keys "n"
                                 :time-or-todo "%U"
                                 :heading "Notes"
                                 :file +org-capture-central-project-notes-file)
                                ("Project changelog"
                                 :keys "c"
                                 :time-or-todo "%U"
                                 :heading "Unreleased"
                                 :file +org-capture-central-project-changelog-file)))))))
  
    (set-org-capture-templates)
    (unless (display-graphic-p)
      (add-hook 'server-after-make-frame-hook
                (defun org-capture-reinitialise-hook ()
                  (when (display-graphic-p)
                    (set-org-capture-templates)
                    (remove-hook 'server-after-make-frame-hook
                                 #'org-capture-reinitialise-hook))))))
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
  Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
  
  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.
  
  TABLE is the alist which should contain entries where the car is a string.
  There should be two types of entries.
  
  1. prefix descriptions like (\"a\" \"Description\")
     This indicates that `a' is a prefix key for multi-letter selection, and
     that there are entries following with keys like \"ab\", \"ax\"…
  
  2. Select-able members must have more than two elements, with the first
     being the string of keys that lead to selecting it, and the second a
     short description string of the item.
  
  The command will then make a temporary buffer listing all entries
  that can be selected with a single key, and all the single key
  prefixes.  When you press the key for a single-letter entry, it is selected.
  When you press a prefix key, the commands (and maybe further prefixes)
  under this key will be shown and offered for selection.
  
  TITLE will be placed over the selection in the temporary buffer,
  PROMPT will be used when prompting for a key.  SPECIALS is an
  alist with (\"key\" \"description\") entries.  When one of these
  is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sublevel prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)
  (setf (alist-get 'height +org-capture-frame-parameters) 15)
  ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
  (setq +org-capture-fn
        (lambda ()
          (interactive)
          (set-window-parameter nil 'mode-line-format 'none)
          (org-capture)))
  (use-package! websocket
    :after org-roam-ui)
  
  (use-package! org-roam-ui
    :commands org-roam-ui-open
    :config (setq org-roam-ui-sync-theme t
                  org-roam-ui-follow t
                  org-roam-ui-update-on-save t
                  org-roam-ui-open-on-start t))
  (setq org-roam-directory "~/Dropbox/Org/slip-box")
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
    :around #'doom-modeline-buffer-file-name ; takes no args
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔(\\1-\\2-\\3) "
         (subst-char-in-string ?_ ?  buffer-file-name))
      (funcall orig-fun)))
  (after! org-roam
    (setq org-roam-capture-ref-templates
          '(("r" "ref" plain "%?"
             :if-new (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %U\n\n${body}\n")
             :unnarrowed t))))
  (defun +yas/org-src-header-p ()
    "Determine whether `point' is within a src-block header or header-args."
    (pcase (org-element-type (org-element-context))
      ('src-block (< (point) ; before code part of the src-block
                     (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                     (forward-line 1)
                                     (point))))
      ('inline-src-block (< (point) ; before code part of the inline-src-block
                            (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                            (search-forward "]{")
                                            (point))))
      ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))
  (defun +yas/org-prompt-header-arg (arg question values)
    "Prompt the user to set ARG header property to one of VALUES with QUESTION.
  The default value is identified and indicated. If either default is selected,
  or no selection is made: nil is returned."
    (let* ((src-block-p (not (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))))
           (default
             (or
              (cdr (assoc arg
                          (if src-block-p
                              (nth 2 (org-babel-get-src-block-info t))
                            (org-babel-merge-params
                             org-babel-default-header-args
                             (let ((lang-headers
                                    (intern (concat "org-babel-default-header-args:"
                                                    (+yas/org-src-lang)))))
                               (when (boundp lang-headers) (eval lang-headers t)))))))
              ""))
           default-value)
      (setq values (mapcar
                    (lambda (value)
                      (if (string-match-p (regexp-quote value) default)
                          (setq default-value
                                (concat value " "
                                        (propertize "(default)" 'face 'font-lock-doc-face)))
                        value))
                    values))
      (let ((selection (consult--read question values :default default-value)))
        (unless (or (string-match-p "(default)$" selection)
                    (string= "" selection))
          selection))))
  (defun +yas/org-src-lang ()
    "Try to find the current language of the src/header at `point'.
  Return nil otherwise."
    (let ((context (org-element-context)))
      (pcase (org-element-type context)
        ('src-block (org-element-property :language context))
        ('inline-src-block (org-element-property :language context))
        ('keyword (when (string-match "^header-args:\\([^ ]+\\)" (org-element-property :value context))
                    (match-string 1 (org-element-property :value context)))))))
  
  (defun +yas/org-last-src-lang ()
    "Return the language of the last src-block, if it exists."
    (save-excursion
      (beginning-of-line)
      (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
        (org-element-property :language (org-element-context)))))
  
  (defun +yas/org-most-common-no-property-lang ()
    "Find the lang with the most source blocks that has no global header-args, else nil."
    (let (src-langs header-langs)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
          (push (+yas/org-src-lang) src-langs))
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+property: +header-args" nil t)
          (push (+yas/org-src-lang) header-langs)))
  
      (setq src-langs
            (mapcar #'car
                    ;; sort alist by frequency (desc.)
                    (sort
                     ;; generate alist with form (value . frequency)
                     (cl-loop for (n . m) in (seq-group-by #'identity src-langs)
                              collect (cons n (length m)))
                     (lambda (a b) (> (cdr a) (cdr b))))))
  
      (car (cl-set-difference src-langs header-langs :test #'string=))))
  (defun +org-syntax-convert-keyword-case-to-lower ()
    "Convert all #+KEYWORDS to #+keywords."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (case-fold-search nil))
        (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
          (unless (s-matches-p "RESULTS" (match-string 0))
            (replace-match (downcase (match-string 0)) t)
            (setq count (1+ count))))
        (message "Replaced %d occurances" count))))
  (use-package! org-wild-notifier
    :hook (org-load . org-wild-notifier-mode)
    :config
    (setq org-wild-notifier-alert-time '(60 30)))
  (use-package! org-menu
    :commands (org-menu)
    :init
    (map! :localleader
          :map org-mode-map
          :desc "Org menu" "M" #'org-menu))
  (cl-defmacro +lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    ;; (setq centaur-lsp 'lsp-mode)
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (setq file-name (make-temp-file "babel-lsp-")))
             (setq buffer-file-name file-name)
             (lsp-deferred)))
         (put ',intern-pre 'function-documentation
              (format "Enable lsp-mode in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))
  
  (defvar +org-babel-lang-list
    '("go" "python" "ipython" "bash" "sh"))
  
  (dolist (lang +org-babel-lang-list)
    (eval `(+lsp-org-babel-enable ,lang)))
  (org-link-set-parameters
   "subfig"
   :follow (lambda (file) (find-file file))
   :face '(:foreground "chocolate" :weight bold :underline t)
   :display 'full
   :export
   (lambda (file desc backend)
     (when (eq backend 'latex)
       (if (string-match ">(\\(.+\\))" desc)
           (concat "\\begin{subfigure}[b]"
                   "\\caption{" (replace-regexp-in-string "\s+>(.+)" "" desc) "}"
                   "\\includegraphics" "[" (match-string 1 desc) "]" "{" file "}" "\\end{subfigure}")
         (format "\\begin{subfigure}\\includegraphics{%s}\\end{subfigure}" desc file)))))
  (org-add-link-type
   "latex" nil
   (lambda (path desc format)
     (cond
      ((eq format 'html)
       (format "<span class=\"%s\">%s</span>" path desc))
      ((eq format 'latex)
       (format "\\%s{%s}" path desc)))))
  (custom-set-faces!
    '(org-document-title :height 1.2))
  
  (custom-set-faces!
    '(outline-1 :weight extra-bold :height 1.25)
    '(outline-2 :weight bold :height 1.15)
    '(outline-3 :weight bold :height 1.12)
    '(outline-4 :weight semi-bold :height 1.09)
    '(outline-5 :weight semi-bold :height 1.06)
    '(outline-6 :weight semi-bold :height 1.03)
    '(outline-8 :weight semi-bold)
    '(outline-9 :weight semi-bold))
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.000 . org-warning)
          (0.500 . org-upcoming-deadline)
          (0.000 . org-upcoming-distant-deadline)))
  (setq org-fontify-quote-and-verse-blocks t)
  (use-package! org-appear
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autoemphasis t
          org-appear-autosubmarkers t
          org-appear-autolinks nil)
    ;; for proper first-time setup, `org-appear--set-elements'
    ;; needs to be run after other hooks have acted.
    (run-at-time nil nil #'org-appear--set-elements))
  (setq org-inline-src-prettify-results '("⟨" . "⟩")
        doom-themes-org-fontify-special-tags nil)
  (use-package! org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶")
          org-modern-table-vertical 5
          org-modern-table-horizontal 2
          org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
          org-modern-footnote (cons nil (cadr org-script-display))
          org-modern-priority t
          org-modern-block t
          org-modern-block-fringe nil
          org-modern-horizontal-rule t
          org-modern-keyword
          '((t                     . t)
            ("title"               . "𝙏")
            ("subtitle"            . "𝙩")
            ("author"              . "𝘼")
            ("email"               . "@")
            ("date"                . "𝘿")
            ("lastmod"             . "✎")
            ("property"            . "☸")
            ("options"             . "⌥")
            ("startup"             . "⏻")
            ("macro"               . "𝓜")
            ("bind"                . #("" 0 1 (display (raise -0.1))))
            ("bibliography"        . "")
            ("print_bibliography"  . #("" 0 1 (display (raise -0.1))))
            ("cite_export"         . "⮭")
            ("print_glossary"      . #("ᴬᶻ" 0 1 (display (raise -0.1))))
            ("glossary_sources"    . #("" 0 1 (display (raise -0.14))))
            ("export_file_name"    . "⇒")
            ("include"             . "⇤")
            ("setupfile"           . "⇐")
            ("html_head"           . "🅷")
            ("html"                . "🅗")
            ("latex_class"         . "🄻")
            ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
            ("latex_header"        . "🅻")
            ("latex_header_extra"  . "🅻⁺")
            ("latex"               . "🅛")
            ("beamer_theme"        . "🄱")
            ("beamer_color_theme"  . #("🄱" 1 2 (display (raise -0.12))))
            ("beamer_font_theme"   . "🄱𝐀")
            ("beamer_header"       . "🅱")
            ("beamer"              . "🅑")
            ("attr_latex"          . "🄛")
            ("attr_html"           . "🄗")
            ("attr_org"            . "⒪")
            ("name"                . "⁍")
            ("header"              . "›")
            ("caption"             . "☰")
            ("RESULTS"             . "🠶")
            ("language"            . "𝙇")
            ("hugo_base_dir"       . "𝐇")
            ("latex_compiler"      . "⟾")
            ("results"             . "🠶")
            ("filetags"            . "#")
            ("created"             . "⏱")
            ("export_select_tags"  . "✔")
            ("export_exclude_tags" . "❌")))
  
    ;; Change faces
    (custom-set-faces! '(org-modern-tag :inherit (region org-modern-label)))
    (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))
  (when (modulep! :ui ligatures)
    (defadvice! +org-init-appearance-h--no-ligatures-a ()
      :after #'+org-init-appearance-h
      (set-ligatures! 'org-mode
                      :name nil
                      :src_block nil
                      :src_block_end nil
                      :quote nil
                      :quote_end nil)))
  (use-package! org-ol-tree
    :commands org-ol-tree
    :config
    (setq org-ol-tree-ui-icon-set
          (if (and (display-graphic-p)
                   (fboundp 'all-the-icons-material))
              'all-the-icons
            'unicode))
    (org-ol-tree-ui--update-icon-set))
  
  (map! :localleader
        :map org-mode-map
        :desc "Outline" "O" #'org-ol-tree)
  (defvar +org-responsive-image-percentage 0.4)
  (defvar +org-responsive-image-width-limits '(400 . 700)) ;; '(min-width . max-width)
  
  (defun +org--responsive-image-h ()
    (when (eq major-mode 'org-mode)
      (setq org-image-actual-width
            (max (car +org-responsive-image-width-limits)
                 (min (cdr +org-responsive-image-width-limits)
                      (truncate (* (window-pixel-width) +org-responsive-image-percentage)))))))
  
  (add-hook 'window-configuration-change-hook #'+org--responsive-image-h)
  (setq org-list-demote-modify-bullet
        '(("+"  . "-")
          ("-"  . "+")
          ("*"  . "+")
          ("1." . "a.")))
  ;; Org styling, hide markup etc.
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ↩"
        org-hide-leading-stars t)
        ;; org-priority-highest ?A
        ;; org-priority-lowest ?E
        ;; org-priority-faces
        ;; '((?A . 'all-the-icons-red)
        ;;   (?B . 'all-the-icons-orange)
        ;;   (?C . 'all-the-icons-yellow)
        ;;   (?D . 'all-the-icons-green)
        ;;   (?E . 'all-the-icons-blue)))
  (setq org-highlight-latex-and-related '(native script entities))
  
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  
  ;; Can be dvipng, dvisvgm, imagemagick
  (setq org-preview-latex-default-process 'dvisvgm)
  
  ;; Define a function to set the format latex scale (to be reused in hooks)
  (defun +org-format-latex-set-scale (scale)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale scale)))
  
  ;; Set the default scale
  (+org-format-latex-set-scale 1.4)
  
  ;; Increase scale in Zen mode
  (when (modulep! :ui zen)
    (add-hook! 'writeroom-mode-enable-hook (+org-format-latex-set-scale 2.0))
    (add-hook! 'writeroom-mode-disable-hook (+org-format-latex-set-scale 1.4)))
  (defun +parse-the-fun (str)
    "Parse the LaTeX environment STR.
  Return an AST with newlines counts in each level."
    (let (ast)
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward
                (rx "\\"
                    (group (or "\\" "begin" "end" "nonumber"))
                    (zero-or-one "{" (group (zero-or-more not-newline)) "}"))
                nil t)
          (let ((cmd (match-string 1))
                (env (match-string 2)))
            (cond ((string= cmd "begin")
                   (push (list :env (intern env)) ast))
                  ((string= cmd "\\")
                   (let ((curr (pop ast)))
                     (push (plist-put curr :newline (1+ (or (plist-get curr :newline) 0))) ast)))
                  ((string= cmd "nonumber")
                   (let ((curr (pop ast)))
                     (push (plist-put curr :nonumber (1+ (or (plist-get curr :nonumber) 0))) ast)))
                  ((string= cmd "end")
                   (let ((child (pop ast))
                         (parent (pop ast)))
                     (push (plist-put parent :childs (cons child (plist-get parent :childs))) ast)))))))
      (plist-get (car ast) :childs)))
  
  (defun +scimax-org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
          (counter -1))
      (setq results
            (cl-loop for (begin . env) in
                     (org-element-map (org-element-parse-buffer) 'latex-environment
                       (lambda (env)
                         (cons
                          (org-element-property :begin env)
                          (org-element-property :value env))))
                     collect
                     (cond
                      ((and (string-match "\\\\begin{equation}" env)
                            (not (string-match "\\\\tag{" env)))
                       (cl-incf counter)
                       (cons begin counter))
                      ((string-match "\\\\begin{align}" env)
                       (cl-incf counter)
                       (let ((p (car (+parse-the-fun env))))
                         ;; Parse the `env', count new lines in the align env as equations, unless
                         (cl-incf counter (- (or (plist-get p :newline) 0)
                                             (or (plist-get p :nonumber) 0))))
                       (cons begin counter))
                      (t
                       (cons begin nil)))))
      (when-let ((number (cdr (assoc (point) results))))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" number)
               (car args)))))
    (apply orig-func args))
  
  (defun +scimax-toggle-latex-equation-numbering (&optional enable)
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (or enable (not (get '+scimax-org-renumber-environment 'enabled)))
        (progn
          (advice-add 'org-create-formula-image :around #'+scimax-org-renumber-environment)
          (put '+scimax-org-renumber-environment 'enabled t)
          (message "LaTeX numbering enabled."))
      (advice-remove 'org-create-formula-image #'+scimax-org-renumber-environment)
      (put '+scimax-org-renumber-environment 'enabled nil)
      (message "LaTeX numbering disabled.")))
  
  (defun +scimax-org-inject-latex-fragment (orig-func &rest args)
    "Advice function to inject latex code before and/or after the equation in a latex fragment.
  You can use this to set \\mathversion{bold} for example to make
  it bolder. The way it works is by defining
  :latex-fragment-pre-body and/or :latex-fragment-post-body in the
  variable `org-format-latex-options'. These strings will then be
  injected before and after the code for the fragment before it is
  made into an image."
    (setf (car args)
          (concat
           (or (plist-get org-format-latex-options :latex-fragment-pre-body) "")
           (car args)
           (or (plist-get org-format-latex-options :latex-fragment-post-body) "")))
    (apply orig-func args))
  
  (defun +scimax-toggle-inject-latex ()
    "Toggle whether you can insert latex in fragments."
    (interactive)
    (if (not (get '+scimax-org-inject-latex-fragment 'enabled))
        (progn
          (advice-add 'org-create-formula-image :around #'+scimax-org-inject-latex-fragment)
          (put '+scimax-org-inject-latex-fragment 'enabled t)
          (message "Inject latex enabled"))
      (advice-remove 'org-create-formula-image #'+scimax-org-inject-latex-fragment)
      (put '+scimax-org-inject-latex-fragment 'enabled nil)
      (message "Inject latex disabled")))
  
  ;; Enable renumbering by default
  (+scimax-toggle-latex-equation-numbering t)
  (use-package! org-fragtog
    :hook (org-mode . org-fragtog-mode))
  (after! org-plot
    (defun org-plot/generate-theme (_type)
      "Use the current Doom theme colours to generate a GnuPlot preamble."
      (format "
  fgt = \"textcolor rgb '%s'\"  # foreground text
  fgat = \"textcolor rgb '%s'\" # foreground alt text
  fgl = \"linecolor rgb '%s'\"  # foreground line
  fgal = \"linecolor rgb '%s'\" # foreground alt line
  
  # foreground colors
  set border lc rgb '%s'
  # change text colors of  tics
  set xtics @fgt
  set ytics @fgt
  # change text colors of labels
  set title @fgt
  set xlabel @fgt
  set ylabel @fgt
  # change a text color of key
  set key @fgt
  
  # line styles
  set linetype 1 lw 2 lc rgb '%s' # red
  set linetype 2 lw 2 lc rgb '%s' # blue
  set linetype 3 lw 2 lc rgb '%s' # green
  set linetype 4 lw 2 lc rgb '%s' # magenta
  set linetype 5 lw 2 lc rgb '%s' # orange
  set linetype 6 lw 2 lc rgb '%s' # yellow
  set linetype 7 lw 2 lc rgb '%s' # teal
  set linetype 8 lw 2 lc rgb '%s' # violet
  
  # palette
  set palette maxcolors 8
  set palette defined ( 0 '%s',\
  1 '%s',\
  2 '%s',\
  3 '%s',\
  4 '%s',\
  5 '%s',\
  6 '%s',\
  7 '%s' )
  "
              (doom-color 'fg)
              (doom-color 'fg-alt)
              (doom-color 'fg)
              (doom-color 'fg-alt)
              (doom-color 'fg)
              ;; colours
              (doom-color 'red)
              (doom-color 'blue)
              (doom-color 'green)
              (doom-color 'magenta)
              (doom-color 'orange)
              (doom-color 'yellow)
              (doom-color 'teal)
              (doom-color 'violet)
              ;; duplicated
              (doom-color 'red)
              (doom-color 'blue)
              (doom-color 'green)
              (doom-color 'magenta)
              (doom-color 'orange)
              (doom-color 'yellow)
              (doom-color 'teal)
              (doom-color 'violet)))
  
    (defun org-plot/gnuplot-term-properties (_type)
      (format "background rgb '%s' size 1050,650"
              (doom-color 'bg)))
  
    (setq org-plot/gnuplot-script-preamble #'org-plot/generate-theme
          org-plot/gnuplot-term-extra #'org-plot/gnuplot-term-properties))
  (use-package! org-phscroll
    :hook (org-mode . org-phscroll-mode))
  (setq bibtex-completion-bibliography +my/biblio-libraries-list
        bibtex-completion-library-path +my/biblio-storage-list
        bibtex-completion-notes-path +my/biblio-notes-path
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))
  (use-package! org-bib
    :commands (org-bib-mode))
  (after! oc
    (setq org-cite-csl-styles-dir +my/biblio-styles-path)
          ;; org-cite-global-bibliography +my/biblio-libraries-list)
  
    (defun +org-ref-to-org-cite ()
      "Simple conversion of org-ref citations to org-cite syntax."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\[cite\\(.*\\):\\([^]]*\\)\\]" nil t)
          (let* ((old (substring (match-string 0) 1 (1- (length (match-string 0)))))
                 (new (s-replace "&" "@" old)))
            (message "Replaced citation %s with %s" old new)
            (replace-match new))))))
  (after! citar
    (setq citar-library-paths +my/biblio-storage-list
          citar-notes-paths  (list +my/biblio-notes-path)
          citar-bibliography  +my/biblio-libraries-list
          citar-symbol-separator "  ")
  
    (when (display-graphic-p)
      (setq citar-symbols
            `((file ,(all-the-icons-octicon "file-pdf"      :face 'error) . " ")
              (note ,(all-the-icons-octicon "file-text"     :face 'warning) . " ")
              (link ,(all-the-icons-octicon "link-external" :face 'org-link) . " ")))))
  
  (use-package! citar-org-roam
    :after citar org-roam
    :no-require
    :config (citar-org-roam-mode)
    :init
    ;; Modified form: https://jethrokuan.github.io/org-roam-guide/
    (defun +org-roam-node-from-cite (entry-key)
      (interactive (list (citar-select-ref)))
      (let ((title (citar-format--entry
                    "${author editor} (${date urldate}) :: ${title}"
                    (citar-get-entry entry-key))))
        (org-roam-capture- :templates
                           '(("r" "reference" plain
                              "%?"
                              :if-new (file+head "references/${citekey}.org"
                                                 ":properties:
  :roam_refs: [cite:@${citekey}]
  :end:
  #+title: ${title}\n")
                              :immediate-finish t
                              :unnarrowed t))
                           :info (list :citekey entry-key)
                           :node (org-roam-node-create :title title)
                           :props '(:finalize find-file)))))
  (setq org-export-headline-levels 5)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-creator-string
        (format "Made with Emacs %s and Org %s" emacs-version (org-release)))
  ;; `org-latex-compilers' contains a list of possible values for the `%latex' argument.
  (setq org-latex-pdf-process
        '("latexmk -shell-escape -pdf -quiet -f -%latex -interaction=nonstopmode -output-directory=%o %f"))
  ;; 'svg' package depends on inkscape, imagemagik and ghostscript
  (when (+all (mapcar 'executable-find '("inkscape" "magick" "gs")))
    (add-to-list 'org-latex-packages-alist '("" "svg")))
  
  (add-to-list 'org-latex-packages-alist '("svgnames" "xcolor"))
  ;; (add-to-list 'org-latex-packages-alist '("" "fontspec")) ;; for xelatex
  ;; (add-to-list 'org-latex-packages-alist '("utf8" "inputenc"))
  ;; Should be configured per document, as a local variable
  ;; (setq org-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  
  ;; Default `minted` options, can be overwritten in file/dir locals
  (setq org-latex-minted-options
        '(("frame"         "lines")
          ("fontsize"      "\\footnotesize")
          ("tabsize"       "2")
          ("breaklines"    "true")
          ("breakanywhere" "true") ;; break anywhere, no just on spaces
          ("style"         "default")
          ("bgcolor"       "GhostWhite")
          ("linenos"       "true")))
  
  ;; Link some org-mode blocks languages to lexers supported by minted
  ;; via (pygmentize), you can see supported lexers by running this command
  ;; in a terminal: `pygmentize -L lexers'
  (dolist (pair '((ipython    "python")
                  (jupyter    "python")
                  (scheme     "scheme")
                  (lisp-data  "lisp")
                  (conf-unix  "unixconfig")
                  (conf-space "unixconfig")
                  (authinfo   "unixconfig")
                  (gdb-script "unixconfig")
                  (conf-toml  "yaml")
                  (conf       "ini")
                  (gitconfig  "ini")
                  (systemd    "ini")))
    (unless (member pair org-latex-minted-langs)
      (add-to-list 'org-latex-minted-langs pair)))
  (after! ox-latex
    (add-to-list
     'org-latex-classes
     '("scr-article"
       "\\documentclass{scrartcl}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("lettre"
       "\\documentclass{lettre}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("blank"
       "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("IEEEtran"
       "\\documentclass{IEEEtran}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("ieeeconf"
       "\\documentclass{ieeeconf}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("sagej"
       "\\documentclass{sagej}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("thesis"
       "\\documentclass[11pt]{book}"
       ("\\chapter{%s}"       . "\\chapter*{%s}")
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")))
  
    (add-to-list
     'org-latex-classes
     '("thesis-fr"
       "\\documentclass[french,12pt,a4paper]{book}"
       ("\\chapter{%s}"       . "\\chapter*{%s}")
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}"))))
  
  (setq org-latex-default-class "article")
  
  ;; org-latex-tables-booktabs t
  ;; org-latex-reference-command "\\cref{%s}")
  (defvar +org-export-to-pdf-main-file nil
    "The main (entry point) Org file for a multi-files document.")
  
  (advice-add
   'org-latex-export-to-pdf :around
   (lambda (orig-fn &rest orig-args)
     (message
      "PDF exported to: %s."
      (let ((main-file (or (bound-and-true-p +org-export-to-pdf-main-file) "main.org")))
        (if (file-exists-p (expand-file-name main-file))
            (with-current-buffer (find-file-noselect main-file)
              (apply orig-fn orig-args))
          (apply orig-fn orig-args))))))
  (setq time-stamp-active t
        time-stamp-start  "#\\+lastmod:[ \t]*"
        time-stamp-end    "$"
        time-stamp-format "%04Y-%02m-%02d")
  
  (add-hook 'before-save-hook 'time-stamp nil)
  (setq org-hugo-auto-set-lastmod t)
)

;; [[file:config.org::*Plain text][Plain text:1]]
(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))
;; Plain text:1 ends here

;; [[file:config.org::*Academic phrases][Academic phrases:1]]
(use-package! academic-phrases
  :commands (academic-phrases
             academic-phrases-by-section))
;; Academic phrases:1 ends here

;; [[file:config.org::*Quarto][Quarto:2]]
(use-package! quarto-mode
  :when QUARTO-P)
;; Quarto:2 ends here

;; [[file:config.org::*French apostrophes][French apostrophes:1]]
(defun +helper--in-buffer-replace (old new)
  "Replace OLD with NEW in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (cnt 0))
      (while (re-search-forward old nil t)
        (replace-match new)
        (setq cnt (1+ cnt)))
      cnt)))

(defun +helper-clear-frenchy-ponctuations ()
  "Replace french apostrophes (’) by regular quotes (')."
  (interactive)
  (let ((chars '((" " . "") ("’" . "'")))
        (cnt 0))
    (dolist (pair chars)
      (setq cnt (+ cnt (+helper--in-buffer-replace (car pair) (cdr pair)))))
    (message "Replaced %d matche(s)." cnt)))
;; French apostrophes:1 ends here

;; [[file:config.org::*Yanking multi-lines paragraphs][Yanking multi-lines paragraphs:1]]
(defun +helper-paragraphized-yank ()
  "Copy, then remove newlines and Org styling (/*_~)."
  (interactive)
  (copy-region-as-kill nil nil t)
  (with-temp-buffer
    (yank)
    ;; Remove newlines, and Org styling (/*_~)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "[\n/*_~]" nil t)
        (replace-match (if (s-matches-p (match-string 0) "\n") " " "") t)))
    (kill-region (point-min) (point-max))))

(map! :localleader
      :map (org-mode-map markdown-mode-map latex-mode-map text-mode-map)
      :desc "Paragraphized yank" "y" #'+helper-paragraphized-yank)
;; Yanking multi-lines paragraphs:1 ends here
