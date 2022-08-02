;;; config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*User information][User information:1]]
(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@fedoraproject.org")
;; User information:1 ends here

;; [[file:config.org::*Secrets][Secrets:1]]
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-do-cache t
      auth-source-cache-expiry 86400 ; All day, defaut is 2h (7200)
      password-cache t
      password-cache-expiry 86400)

;; Set my GPG key as the default key
(setq-default epa-file-encrypt-to '("F808A020A3E1AC37"))
;; Secrets:1 ends here

;; [[file:config.org::*File deletion][File deletion:1]]
(setq-default delete-by-moving-to-trash t
              trash-directory nil) ;; Use freedesktop.org trashcan
;; File deletion:1 ends here

;; [[file:config.org::*Window][Window:1]]
(setq-default window-combination-resize t)
;; Window:1 ends here

;; [[file:config.org::*Messages buffer][Messages buffer:1]]
(defvar +messages-buffer-auto-tail--enabled nil)

(defun +messages-buffer-auto-tail--advice (&rest arg)
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

(defun +messages-buffer-toggle-auto-tail ()
  "Auto tail the '*Messages*' buffer."
  (interactive)
  ;; Add/remove an advice from the 'message' function.
  (cond (+messages-buffer-auto-tail--enabled
         (advice-remove 'message '+messages-buffer-auto-tail--advice)
         (setq +messages-buffer-auto-tail--enabled nil)
         (message "+messages-buffer-auto-tail: Disabled."))
        (t
         (advice-add 'message :after '+messages-buffer-auto-tail--advice)
         (setq +messages-buffer-auto-tail--enabled t)
         (message "+messages-buffer-auto-tail: Enabled."))))
;; Messages buffer:1 ends here

;; [[file:config.org::*Split defaults][Split defaults:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Split defaults:1 ends here

;; [[file:config.org::*Split defaults][Split defaults:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
;; Split defaults:2 ends here

;; [[file:config.org::*Undo and auto-save][Undo and auto-save:1]]
(setq undo-limit 80000000   ;; Raise undo-limit to 80Mb
      evil-want-fine-undo t ;; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t   ;; Nobody likes to lose work, I certainly don't
      scroll-preserve-screen-position 'always ;; Don't have `point' jump around
      scroll-margin 2)      ;; It's nice to maintain a little margin
;; Undo and auto-save:1 ends here

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

;; [[file:config.org::*LanguageTool Server][LanguageTool Server:1]]
(when LANGUAGETOOL-P
  (defvar +languagetool--process-name "ltex-languagetool-server")

  (defun +languagetool-server-start (&optional port)
    "Start LanguageTool server with PORT."
    (interactive)
    (if (process-live-p (get-process +languagetool--process-name))
        (message "LanguageTool server already running.")
      (when (start-process
             +languagetool--process-name
             " *LanguageTool server*"
             "languagetool"
             "--http"
             "--port" (format "%s" (or port 8081))
             "--languageModel" "/usr/share/ngrams")
        (message "Started LanguageTool server."))))

  (defun +languagetool-server-stop ()
    "Stop the LanguageTool server."
    (interactive)
    (if (process-live-p (get-process +languagetool--process-name))
        (when (kill-process +languagetool--process-name)
          (message "Stopped LanguageTool server."))
      (message "No LanguageTool server running.")))

  (defun +languagetool-server-restart (&optional port)
    "Restart the LanguageTool server with PORT, start new instance if not running."
    (interactive)
    (when (process-live-p (get-process +languagetool--process-name))
      (+languagetool-server-stop))
    (sit-for 5)
    (+languagetool-server-start port)))
;; LanguageTool Server:1 ends here

;; [[file:config.org::*Initialization][Initialization:1]]
(defun +greedily-do-daemon-setup ()
  (require 'org)
  ;; mu4e
  (when (and (featurep! :email mu4e) (require 'mu4e nil t))
    (setq mu4e-confirm-quit t
          +mu4e-lock-greedy t
          +mu4e-lock-relaxed t)
    (+mu4e-lock-start 'mu4e--start))

  ;; RSS
  (when (and (featurep! :app rss) (require 'elfeed nil t))
    (run-at-time nil (* 8 60 60) #'elfeed-update))

  ;; LanguageTool
  (when (and LANGUAGETOOL-P nil) ;; disabled, too heavy, enable on demand
    (unless (ignore-errors (+languagetool-server-start))
      (message "LanguageTool Server cannot be launched at daemon startup."))))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'+greedily-do-daemon-setup)
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))
;; Initialization:1 ends here

;; [[file:config.org::*Save recent files][Save recent files:1]]
(when (daemonp)
  (add-hook! '(delete-frame-functions delete-terminal-functions)
    (recentf-save-list)))
;; Save recent files:1 ends here

;; [[file:config.org::*Font][Font:1]]
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font Mono" :size 22)
      doom-big-font (font-spec :family "FantasqueSansMono Nerd Font Mono" :size 32)
      doom-variable-pitch-font (font-spec :family "Andika") ;; inherits the :size from doom-font
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Input Serif" :weight 'light))
;; Font:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
(setq doom-theme 'doom-vibrant)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)

;; By default 'doom-vibrant' uses red faces to mark modified file in modeline,
;; lets change it to orange.
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
;; Theme:1 ends here

;; [[file:config.org::*Clock][Clock:1]]
(after! doom-modeline
  (setq display-time-string-forms
        '((propertize (concat " ⌛ " 24-hours ":" minutes))))
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
(setq doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-state-icon t)
;; Mode line customization:1 ends here

;; [[file:config.org::*Custom splash image][Custom splash image:1]]
(setq fancy-splash-image (expand-file-name "assets/emacs-e.png" doom-private-dir))
;; Custom splash image:1 ends here

;; [[file:config.org::*Dashboard][Dashboard:1]]
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook!   '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook!  '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(defun +doom/open-private-config-org ()
  (interactive)
  (when (file-directory-p doom-private-dir)
    (find-file (expand-file-name "config.org" doom-private-dir))))

;; (setq +doom-dashboard-menu-sections
;;   '(("Reload last session"
;;      :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
;;      :when (cond ((featurep! :ui workspaces)
;;                   (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
;;                  ((require 'desktop nil t)
;;                   (file-exists-p (desktop-full-file-name))))
;;      :face (:inherit (doom-dashboard-menu-title bold))
;;      :action doom/quickload-session)
;;     ("Open mailbox"
;;      :icon (all-the-icons-octicon "mail" :face 'doom-dashboard-menu-title)
;;      :action =mu4e)
;;     ("Open org-agenda"
;;      :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
;;      :when (fboundp 'org-agenda)
;;      :action org-agenda)
;;     ("Jump to bookmark"
;;      :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
;;      :action bookmark-jump)
;;     ("Open config.org"
;;      :icon (all-the-icons-fileicon "config" :face 'doom-dashboard-menu-title)
;;      :when (file-directory-p doom-private-dir)
;;      :action +doom/open-private-config-org)))

(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file" :ne "f" #'find-file
        :desc "Recent files" :ne "r" #'consult-recent-file
        :desc "Config dir" :ne "C" #'doom/open-private-config
        :desc "Open config.org" :ne "c" #'+doom/open-private-config-org
        :desc "Open dotfile" :ne "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)" :ne "n" #'org-roam-node-find
        :desc "Switch buffer" :ne "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ne "B" #'consult-buffer
        :desc "IBuffer" :ne "i" #'ibuffer
        :desc "Previous buffer" :ne "p" #'previous-buffer
        :desc "Email" :ne "m" #'=mu4e
        :desc "Quit" :ne "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings" :ne "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))

(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
;; Dashboard:1 ends here

;; [[file:config.org::*Which key][Which key:1]]
(setq which-key-idle-delay 0.5 ;; Default is 1.0
      which-key-idle-secondary-delay 0.05) ;; Default is nil
;; Which key:1 ends here

;; [[file:config.org::*Which key][Which key:2]]
(setq which-key-allow-multiple-replacements t)

(after! which-key
  (pushnew! which-key-replacement-alist
            '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
            '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))
;; Which key:2 ends here

;; [[file:config.org::*Window title][Window title:1]]
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p) " ◉ %s" " ● %s") project-name))))))
;; Window title:1 ends here

;; [[file:config.org::*Fringe][Fringe:1]]
;; (after! lsp-mode
;;   (add-hook 'lsp-mode-hook (lambda () (set-fringe-mode '(15 . 15)))))

(setq-default left-fringe-width 25
              right-fringe-width 25)
;; Fringe:1 ends here

;; [[file:config.org::*Vertico][Vertico:1]]
(after! vertico-posframe
  (setq vertico-posframe-parameters '((left-fringe . 12) (right-fringe . 14))
        vertico-posframe-border-width 3))
;; Vertico:1 ends here

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

;; [[file:config.org::*SVG tag][SVG tag:2]]
(use-package! svg-tag-mode
  :commands svg-tag-mode
  :config
  (setq svg-tag-tags
      '(("^\\*.* .* \\(:[A-Za-z0-9]+\\)" .
         ((lambda (tag) (svg-tag-make)
                    tag
                    :beg 1
                    :font-family "Roboto Mono"
                    :font-size 6
                    :height 0.6
                    :padding 0
                    :margin 0)))
        ("\\(:[A-Za-z0-9]+:\\)$" .
         ((lambda (tag) (svg-tag-make)
                    tag
                    :beg 1
                    :end -1
                    :font-family "Roboto Mono"
                    :font-size 6
                    :height 0.6
                    :padding 0
                    :margin 0))))))
;; SVG tag:2 ends here

;; [[file:config.org::*Focus][Focus:2]]
(use-package! focus
  :commands focus-mode)
;; Focus:2 ends here

;; [[file:config.org::*Smooth scrolling][Smooth scrolling:2]]
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode 1)
  (use-package! good-scroll
    :config (good-scroll-mode 1)))
;; Smooth scrolling:2 ends here

;; [[file:config.org::*All the icons][All the icons:1]]
(after! all-the-icons
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))
;; All the icons:1 ends here

;; [[file:config.org::*Scratch buffer][Scratch buffer:1]]
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode)
;; Scratch buffer:1 ends here

;; [[file:config.org::*Mouse buttons][Mouse buttons:1]]
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
;; Mouse buttons:1 ends here

;; [[file:config.org::*Page break lines][Page break lines:2]]
(use-package! page-break-lines
  :diminish
  :init (global-page-break-lines-mode))
;; Page break lines:2 ends here

;; [[file:config.org::*Binary files][Binary files:1]]
(defun +hexl/buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion (goto-char (point-min))
                    (search-forward (string ?\x00) nil t 1))))

(defun +hexl/hexl-if-binary ()
  "If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'."
  (interactive)
  (unless (eq major-mode 'hexl-mode)
    (when (+hexl/buffer-binary-p)
      (hexl-mode))))

(add-to-list 'magic-fallback-mode-alist '(+hexl/buffer-binary-p . hexl-mode) t)
;; Binary files:1 ends here

;; [[file:config.org::*Very large files][Very large files:2]]
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)
;; Very large files:2 ends here

;; [[file:config.org::*Evil][Evil:2]]
(after! evil
  (setq evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring
;; Evil:2 ends here

;; [[file:config.org::*Aggressive indent][Aggressive indent:2]]
(use-package! aggressive-indent
  :commands (aggressive-indent-mode))
;; Aggressive indent:2 ends here

;; [[file:config.org::*YASnippet][YASnippet:1]]
(setq yas-triggers-in-field t)
;; YASnippet:1 ends here

;; [[file:config.org::*Asynchronous tangling][Asynchronous tangling:1]]
(defvar +literate-tangle--proc nil)
(defvar +literate-tangle--proc-start-time nil)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (unless (getenv "__NOTANGLE")
    (let ((default-directory doom-private-dir))
      (when +literate-tangle--proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-tangle--proc #'ignore)
        (kill-process +literate-tangle--proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-tangle--proc-start-time (float-time)
            +literate-tangle--proc
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-confirm-babel-evaluate nil \
      org-inhibit-startup t \
      org-mode-hook nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      vc-handled-backends nil \
      org-startup-folded nil \
      org-startup-indented nil) \
(org-babel-tangle-file \"%s\" \"%s\"))"
                                   +literate-config-file
                                   (expand-file-name (concat doom-module-config-file ".el")))))
      (set-process-sentinel +literate-tangle--proc #'+literate-tangle--sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-tangle--sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (pop-to-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))))

(defun +literate-tangle-check-finished ()
  (when (and (process-live-p +literate-tangle--proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))

(add-hook! 'kill-emacs-hook #'+literate-tangle-check-finished)
;; Asynchronous tangling:1 ends here

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
;; Run `M-x projectile-project-search-path' to reload paths from this variable
(setq projectile-project-search-path
      '("~/PhD/workspace"
        "~/PhD/workspace-no"
        "~/PhD/papers"
        "~/PhD/workspace-no/ez-wheel/swd-starter-kit-repo"
        "~/Projects/foss"))

(setq projectile-ignored-projects
      '("/tmp"
        "~/"
        "~/.cache"
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

;; [[file:config.org::*Enable some useful UI stuff][Enable some useful UI stuff:1]]
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
;; Enable some useful UI stuff:1 ends here

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
  (setq lsp-vhdl-server-path "~/Projects/foss/rust_hdl/target/release/vhdl_ls"
        lsp-vhdl-server 'vhdl-ls
        lsp-vhdl--params nil)
  (require 'lsp-vhdl))
;; VHDL:1 ends here

;; [[file:config.org::*SonarLint][SonarLint:2]]
;; TODO: configure it, for the moment, it seems that it doesn't support C/C++
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
(setq +ligatures-extras-in-modes '(not c-mode c++-mode rust-mode python-mode))
;; Ligatures:1 ends here

;; [[file:config.org::*Spell-Fu][Spell-Fu:1]]
(after! spell-fu
  (defun +spell-fu-register-dictionary (lang)
    "Add `LANG` to spell-fu multi-dict, with a personal dictionary."
    ;; Add the dictionary
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary lang))
    (let ((personal-dict-file (expand-file-name (format "aspell.%s.pws" lang) doom-private-dir)))
      ;; Create an empty personal dictionary if it doesn't exists
      (unless (file-exists-p personal-dict-file) (write-region "" nil personal-dict-file))
      ;; Add the personal dictionary
      (spell-fu-dictionary-add (spell-fu-get-personal-dictionary (format "%s-personal" lang) personal-dict-file))))

  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (+spell-fu-register-dictionary "en")
              (+spell-fu-register-dictionary "fr"))))
;; Spell-Fu:1 ends here

;; [[file:config.org::*Guess language][Guess language:2]]
(use-package! guess-language
  :config
  (setq guess-language-languages '(en fr ar)
        guess-language-min-paragraph-length 35
        guess-language-langcodes '((en . ("en_US" "English" "🇺🇸" "English"))
                                   (fr . ("francais" "French" "🇫🇷" "Français"))
                                   (ar . ("arabic" "Arabic" "🇩🇿" "Arabic"))))
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
  :when (featurep! :tools lsp +eglot)
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
  :when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
  :commands (+lsp-grammarly-load +lsp-grammarly-toggle)
  :init
  (after! lsp-mode
    ;; Disable by default
    (add-to-list 'lsp-disabled-clients 'grammarly-ls))

  (defun +lsp-grammarly-load ()
    "Load Grammarly LSP server for LSP Mode."
    (interactive)
    (require 'lsp-grammarly)
    (lsp-deferred)) ;; or (lsp)

  (defun +lsp-grammarly-toggle ()
    "Enable/disable Grammarly LSP."
    (interactive)
    (if (member 'grammarly-ls lsp-disabled-clients)
        (progn
          (setq lsp-disabled-clients (remove 'grammarly-ls lsp-disabled-clients))
          (message "Enabled grammarly-ls"))
      (progn
        (add-to-list 'lsp-disabled-clients 'grammarly-ls)
        (message "Disabled grammarly-ls"))))

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
  (setq grammalecte-settings-file (expand-file-name "grammalecte/grammalecte-cache.el" doom-etc-dir)
        grammalecte-python-package-directory (expand-file-name "grammalecte/grammalecte" doom-etc-dir))
  (setq flycheck-grammalecte-report-spellcheck t
        flycheck-grammalecte-report-grammar t
        flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp nil
        flycheck-grammalecte-report-nbsp nil
        flycheck-grammalecte-filters
        '("(?m)^# ?-*-.+$"
          ;; Ignore LaTeX equations (inline and block)
          "\\$.*?\\$"
          "(?s)\\\\begin{equation}.*?\\\\end{equation}"))

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

;; [[file:config.org::*Doom's =:checkers grammar=][Doom's =:checkers grammar=:1]]
(after! langtool
  (when LANGUAGETOOL-P
    ;; Use the server
    (setq langtool-language-tool-server-jar nil
          langtool-http-server-host "localhost"
          langtool-http-server-port 8081))

  (setq langtool-default-language "auto"
        langtool-mother-tongue "ar"
        langtool-disabled-rules nil))

;; Keybinding for `langtool' (of module `:checkers grammar')
(map! :leader :prefix ("l" . "custom")
      (:when (featurep! :checkers grammar)
       :prefix ("l" . "langtool")
       :desc "Check"             "l" #'langtool-check
       :desc "Correct buffer"    "b" #'langtool-correct-buffer
       :desc "Done checking"     "d" #'langtool-check-done
       :desc "Show msg at point" "m" #'langtool-show-message-at-point
       :desc "Next error"        "n" #'langtool-goto-next-error
       :desc "Previous error"    "p" #'langtool-goto-previous-error
       (:prefix ("s" . "server")
        :desc "Start server"     "s" #'+languagetool-server-start
        :desc "Stop server"      "q" #'+languagetool-server-stop
        :desc "Restart server"   "r" #'+languagetool-server-restart)))
;; Doom's =:checkers grammar=:1 ends here

;; [[file:config.org::*LTeX][LTeX:2]]
(use-package! lsp-ltex
  :commands (+lsp-ltex-load
             +lsp-ltex-toggle)
  :init
  (setq lsp-ltex-java-force-try-system-wide t
        lsp-ltex-check-frequency "save"
        lsp-ltex-language "auto"
        lsp-ltex-mother-tongue "ar"
        lsp-ltex-additional-rules-language-model "/usr/share/ngrams"
        lsp-ltex-disabled-rules
        '(:fr ["WHITESPACE" "FRENCH_WHITESPACE" "DEUX_POINTS_ESPACE"]
          :en ["WHITESPACE"]))

  ;; If LanguageTool is installed, use it over the LT bundeled with ltex-ls
  ;; In this way, I can configure it to use the extra stuff installed from the
  ;; pacakge manager (like ngrams)
  (when LANGUAGETOOL-P
    (setq lsp-ltex-languagetool-http-server-uri "http://localhost:8081"))

  (after! lsp-mode
    ;; Disable by default
    (add-to-list 'lsp-disabled-clients 'ltex-ls))

  (defun +lsp-ltex-load ()
    "Load LTeX LSP server."
    (interactive)
    (require 'lsp-ltex)
    (lsp-deferred))

  (defun +lsp-ltex-toggle ()
    "Enable/disable LTeX LSP."
    (interactive)
    (if (member 'ltex-ls lsp-disabled-clients)
        (progn
          (setq lsp-disabled-clients (remove 'ltex-ls lsp-disabled-clients))
          (when LANGUAGETOOL-P (+languagetool-server-start))
          (+lsp-ltex-load)
          (message "Enabled ltex-ls"))
      (progn
        (add-to-list 'lsp-disabled-clients 'ltex-ls)
        (lsp-disconnect)
        (message "Disabled ltex-ls"))))

  (map! :leader :prefix ("l" . "custom")
        (:prefix "l"
         :desc "Toggle LTeX LS" "t" #'+lsp-ltex-toggle))

  :config
  (set-lsp-priority! 'ltex-ls 2)
  (setq flycheck-checker-error-threshold 1000))
;; LTeX:2 ends here

;; [[file:config.org::*Flycheck][Flycheck:2]]
(use-package! flycheck-languagetool
  :when LANGUAGETOOL-P
  :hook (text-mode . flycheck-languagetool-setup)
  :init
  (setq flycheck-languagetool-server-command '("languagetool" "--http")
        flycheck-languagetool-language "auto"
        ;; See https://languagetool.org/http-api/swagger-ui/#!/default/post_check
        flycheck-languagetool-check-params
        '(("disabledRules" . "FRENCH_WHITESPACE,WHITESPACE,DEUX_POINTS_ESPACE")
          ("motherTongue" . "ar"))))
;; Flycheck:2 ends here

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
  (when (featurep! :completion company)
    (defun +chezmoi--company-backend-h ()
      (require 'chezmoi-company)
      (if chezmoi-mode
          (add-to-list 'company-backends 'chezmoi-company-backend)
        (delete 'chezmoi-company-backend 'company-backends)))

    (add-hook 'chezmoi-mode-hook #'+chezmoi--company-backend-h))

  ;; Integrate with evil mode by toggling template display when entering insert mode.
  (when (featurep! :editor evil)
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
  (message "Unmounted private directory."))
;; eCryptfs:1 ends here

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
  (setq osm-tile-directory (expand-file-name "osm" doom-etc-dir))
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

;; [[file:config.org::*Emacs Application Framework][Emacs Application Framework:1]]
(defconst EAF-DIR (expand-file-name "emacs-application-framework" doom-etc-dir))

(use-package! eaf
  :when EAF-P
  :load-path EAF-DIR
  :commands (eaf-open eaf-open-browser eaf-open-jupyter eaf-open-mail-as-html)
  :init
  (defvar +eaf-enabled-apps
    '(org mail browser mindmap jupyter org-previewer markdown-previewer))
  ;;  file-manager file-browser
  ;;  file-sender music-player video-player
  ;;  git image-viewer

  :config
  ;; Generic
  (setq eaf-start-python-process-when-require t
        eaf-kill-process-after-last-buffer-closed t
        eaf-fullscreen-p nil)

  ;; Debug
  (setq eaf-enable-debug nil)

  ;; Web engine
  (setq eaf-webengine-font-family "FantasqueSansMono Nerd Font Mono"
        eaf-webengine-fixed-font-family "FantasqueSansMono Nerd Font Mono"
        eaf-webengine-serif-font-family "FantasqueSansMono Nerd Font Mono"
        eaf-webengine-font-size 14
        eaf-webengine-fixed-font-size 14
        eaf-webengine-download-path "~/Downloads"
        eaf-webengine-enable-plugin t
        eaf-webengine-enable-javascript t
        eaf-webengine-enable-javascript-access-clipboard t
        eaf-webengine-enable-scrollbar t
        eaf-webengine-default-zoom 1.25
        eaf-webengine-scroll-step 200)

  (when (display-graphic-p)
    (require 'eaf-all-the-icons))

  ;; Browser settings
  (when (member 'browser +eaf-enabled-apps)
    (setq eaf-browser-continue-where-left-off t
          eaf-browser-dark-mode "follow"
          eaf-browser-enable-adblocker t
          eaf-browser-enable-autofill nil
          eaf-browser-remember-history t
          eaf-browser-ignore-history-list '("google.com/search" "file://")
          eaf-browser-text-selection-color "auto"
          eaf-browser-translate-language "fr"
          eaf-browser-blank-page-url "https://www.duckduckgo.com"
          eaf-browser-chrome-history-file "~/.config/google-chrome/Default/History"
          eaf-browser-default-search-engine "duckduckgo"
          eaf-browser-continue-where-left-off nil)

    (require 'eaf-browser)

    ;; Make EAF Browser my default browser
    (setq browse-url-browser-function #'eaf-open-browser)
    (defalias 'browse-web #'eaf-open-browser))

  ;; File manager settings
  (when (member 'file-manager +eaf-enabled-apps)
    (setq eaf-file-manager-show-preview nil
          eaf-find-alternate-file-in-dired t
          eaf-file-manager-show-hidden-file t
          eaf-file-manager-show-icon t)
    (require 'eaf-file-manager))

  ;; File Browser
  (when (member 'file-browser +eaf-enabled-apps)
    (require 'eaf-file-browser))

  ;; PDF Viewer settings
  (when (member 'pdf-viewer +eaf-enabled-apps)
    (setq eaf-pdf-dark-mode "follow"
          eaf-pdf-show-progress-on-page nil
          eaf-pdf-dark-exclude-image t
          eaf-pdf-notify-file-changed t)
    (require 'eaf-pdf-viewer)

    (after! org
      ;; Use EAF PDF Viewer in Org
      (defun +eaf-org-open-file-fn (file &optional link)
        "An wrapper function on `eaf-open'."
        (eaf-open file))

      ;; use `emacs-application-framework' to open PDF file: link
      (add-to-list 'org-file-apps '("\\.pdf\\'" . +eaf-org-open-file-fn)))

    (after! latex
      ;; Link EAF with the LaTeX compiler in emacs. When a .tex file is open,
      ;; the Command>Compile and view (C-c C-a) option will compile the .tex
      ;; file into a .pdf file and display it using EAF. Double clicking on the
      ;; PDF side jumps to editing the clicked section.
      (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
      (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
      (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

  ;; Org
  (when (member 'rss-reader +eaf-enabled-apps)
    (setq eaf-rss-reader-split-horizontally nil
          eaf-rss-reader-web-page-other-window t)
    (require 'eaf-org))

  ;; Org
  (when (member 'org +eaf-enabled-apps)
    (require 'eaf-org))

  ;; Mail
  (when (member 'mail +eaf-enabled-apps)
    (require 'eaf-mail))

  ;; Org Previewer
  (when (member 'org-previewer +eaf-enabled-apps)
    (setq eaf-org-dark-mode "follow")
    (require 'eaf-org-previewer))

  ;; Markdown Previewer
  (when (member 'markdown-previewer +eaf-enabled-apps)
    (setq eaf-markdown-dark-mode "follow")
    (require 'eaf-markdown-previewer))

  ;; Jupyter
  (when (member 'jupyter +eaf-enabled-apps)
    (setq eaf-jupyter-dark-mode "follow"
          eaf-jupyter-font-family "JuliaMono"
          eaf-jupyter-font-size 13)
    (require 'eaf-jupyter))

  ;; Mindmap
  (when (member 'mindmap +eaf-enabled-apps)
    (setq eaf-mindmap-dark-mode "follow"
          eaf-mindmap-save-path "~/Dropbox/Mindmap")
    (require 'eaf-mindmap))

  ;; File Sender
  (when (member 'file-sender +eaf-enabled-apps)
    (require 'eaf-file-sender))

  ;; Music Player
  (when (member 'music-player +eaf-enabled-apps)
    (require 'eaf-music-player))

  ;; Video Player
  (when (member 'video-player +eaf-enabled-apps)
    (require 'eaf-video-player))

  ;; Image Viewer
  (when (member 'image-viewer +eaf-enabled-apps)
    (require 'eaf-image-viewer))

  ;; Git
  (when (member 'git +eaf-enabled-apps)
    (require 'eaf-git))

  ;; EVIL keybindings for Doom
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
  (add-hook! 'pdf-view-mode-hook (pdf-view-midnight-minor-mode 1)))
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

;; [[file:config.org::*e-Books =nov=][e-Books =nov=:2]]
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
;; e-Books =nov=:2 ends here

;; [[file:config.org::*News feed =elfeed=][News feed =elfeed=:1]]
(setq elfeed-feeds
      '("https://this-week-in-rust.org/rss.xml"
        "https://www.omgubuntu.co.uk/feed"
        "https://itsfoss.com/feed"
        "https://linuxhandbook.com/feed"
        "https://spectrum.ieee.org/rss/robotics/fulltext"
        "https://spectrum.ieee.org/rss/aerospace/fulltext"
        "https://spectrum.ieee.org/rss/computing/fulltext"
        "https://spectrum.ieee.org/rss/blog/automaton/fulltext"
        "https://developers.redhat.com/blog/feed"
        "https://lwn.net/headlines/rss"))
;; News feed =elfeed=:1 ends here

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

;; [[file:config.org::*mu4e][mu4e:2]]
(after! mu4e
  (require 'org-msg)
  (require 'smtpmail)
  (require 'mu4e-contrib)
  (require 'mu4e-icalendar)
  (require 'org-agenda)

  ;; Common parameters
  (setq mu4e-update-interval (* 3 60) ;; Every 3 min
        mu4e-index-update-error-warning nil ;; Do not show warning after update
        mu4e-get-mail-command "mbsync -a" ;; Not needed, as +mu4e-backend is 'mbsync by default
        mu4e-main-hide-personal-addresses t ;; No need to display a long list of my own addresses!
        mu4e-attachment-dir (expand-file-name "~/Maildir/attachements")
        mu4e-sent-messages-behavior 'sent ;; Save sent messages
        mu4e-context-policy 'pick-first   ;; Start with the first context
        mu4e-compose-context-policy 'ask) ;; Always ask which context to use when composing a new mail

  ;; Use msmtp instead of smtpmail
  (setq sendmail-program "/usr/bin/msmtp"
        message-sendmail-f-is-evil t
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("--read-envelope-from") ;; "--read-recipients"
        message-send-mail-function #'message-send-mail-with-sendmail
        send-mail-function #'smtpmail-send-it
        mail-specify-envelope-from t
        mail-envelope-from 'header)

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
        mu4e-headers-results-limit 1000
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
      (concat "(" (s-join (concat " " oper " ") (mapcar (lambda (addr) (format "%s:%s" query addr)) arg-list)) ")"))

    ;; Build a query to match mails send from "me" with "me" in BCC
    (let ((bcc-query (+mu-long-query "bcc" "or" +my-addresses))
          (from-query (+mu-long-query "from" "or" +my-addresses)))
      (add-to-list
       'mu4e-bookmarks
       (list :name "My black copies" :query (format "%s and %s" from-query bcc-query) :key ?k) t)))

  ;; Use a nicer icon in alerts
  (setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/mail-client.svg")

  ;; Org-Msg stuff
  ;; org-msg-[signature|greeting-fmt] are separately set for each account
  (map! :map org-msg-edit-mode-map
        :after org-msg
        :n "G" #'org-msg-goto-body)

  ;; I like to always BCC myself
  (defun +bbc-me ()
    "Add my email to BCC."
    (save-excursion (message-add-header (format "Bcc: %s\n" user-mail-address))))

  (add-hook 'mu4e-compose-mode-hook '+bbc-me)

  ;; FIXME: I constantly get a non systematic error after sending a mail.
  ;; >> Error (message-sent-hook): Error running hook "undo" because:
  ;; >> (error Unrecognized entry in undo list undo-tree-canary)
  ;; It is triggered by the 'message-sent-hook', so lets remove the 'undo'
  ;; command from the hook, we can do this before sending the message via
  ;; the 'message-send-hook'.
  (add-hook 'message-send-hook ;; Befor sending the message
            ;; Remove the problematic 'undo' hook.
            (lambda () (remove-hook 'message-sent-hook 'undo t)))

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
;; mu4e:2 ends here

;; [[file:config.org::*MPD, MPC, and MPV][MPD, MPC, and MPV:1]]
;; Not sure if it is required!
(after! mpc
  (setq mpc-host "localhost:6600"))
;; MPD, MPC, and MPV:1 ends here

;; [[file:config.org::*MPD, MPC, and MPV][MPD, MPC, and MPV:2]]
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
;; MPD, MPC, and MPV:2 ends here

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
   (t
    (setq +emms-notifier-function '+notify-via-messages)))

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
                                +emms-notification-icon)))

  ;; MPV and Youtube integration
  (when MPV-P
    (add-to-list 'emms-player-list 'emms-player-mpv t)
    (emms-player-set
     emms-player-mpv
     'regex
     (rx (or (: "https://" (* nonl) "youtube.com" (* nonl))
             (+ (? (or "https://" "http://"))
                (* nonl)
                (regexp (eval (emms-player-simple-regexp
                               "mp4" "mov" "wmv" "webm" "flv" "avi" "mkv")))))))

    (setq +youtube-dl-quality-list
          '("bestvideo[height<=720]+bestaudio/best[height<=720]"
            "bestvideo[height<=480]+bestaudio/best[height<=480]"
            "bestvideo[height<=1080]+bestaudio/best[height<=1080]"))

    (setq +default-emms-player-mpv-parameters
          '("--quiet" "--really-quiet" "--no-audio-display"))

    (defun +set-emms-mpd-youtube-quality (quality)
      (interactive "P")
      (unless quality
        (setq quality (completing-read "Quality: " +youtube-dl-quality-list nil t)))
      (setq emms-player-mpv-parameters
            `(,@+default-emms-player-mpv-parameters ,(format "--ytdl-format=%s" quality))))

    (+set-emms-mpd-youtube-quality (car +youtube-dl-quality-list))

    (defun +get-youtube-url (link)
      (let ((watch-id (cadr
                       (assoc "watch?v"
                              (url-parse-query-string
                               (substring
                                (url-filename
                                 (url-generic-parse-url link))
                                1))))))
        (concat "https://www.youtube.com/watch?v=" watch-id)))))

;; Example, to be used in an EMMS Playlist
;; (let ((track (emms-track 'url (+get-youtube-url "https://www.youtube.com/watch?v=Wh-7Kg-jVLg&list=PLBsIgVvbrncChqmejIOyA-Xp_dcywQQln"))))
;;   (emms-track-set track 'info-title "Vid")
;;   (emms-playlist-insert-track track))
;; EMMS:1 ends here

;; [[file:config.org::*Elfeed :heart: MPV][Elfeed :heart: MPV:2]]
(after! (elfeed emms)
  (when MPV-P
    ;; Integration with Elfeed
    (define-emms-source elfeed (entry)
      (let ((track (emms-track
                    'url (+get-youtube-url (elfeed-entry-link entry)))))
        (emms-track-set track 'info-title (elfeed-entry-title entry))
        (emms-playlist-insert-track track)))

    (defun +elfeed-add-emms-youtube ()
      (interactive)
      (emms-add-elfeed elfeed-show-entry)
      (elfeed-tag elfeed-show-entry 'watched)
      (elfeed-show-refresh))

    (defun +elfeed-search-filter-source (entry)
      "Filter elfeed search buffer by the feed under cursor."
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-search-set-filter
         (concat
          "@6-months-ago "
          "+unread "
          "="
          (replace-regexp-in-string
           (rx "?" (* not-newline) eos)
           ""
           (elfeed-feed-url (elfeed-entry-feed entry)))))))))
;; Elfeed :heart: MPV:2 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
(map! :leader :prefix ("l" . "custom")
      (:when (featurep! :app emms)
       :prefix ("m" . "media")
       :desc "Playlist go"                 "g" #'emms-playlist-mode-go
       :desc "Add playlist"                "D" #'emms-add-playlist
       :desc "Toggle random playlist"      "r" #'emms-toggle-random-playlist
       :desc "Add directory"               "d" #'emms-add-directory
       :desc "Add file"                    "f" #'emms-add-file
       :desc "Smart browse"                "b" #'emms-smart-browse
       :desc "Play/Pause"                  "p" #'emms-pause
       :desc "Start"                       "S" #'emms-start
       :desc "Start"                       "S" #'emms-start
       :desc "Stop"                        "s" #'emms-stop))
;; Keybindings:1 ends here

;; [[file:config.org::*Keybindings][Keybindings:2]]
(map! :leader
      :prefix ("l m")
      (:when (and (featurep! :app emms) MPD-P)
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

;; [[file:config.org::*File templates][File templates:1]]
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)
;; File templates:1 ends here

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

;; [[file:config.org::*Vim][Vim:2]]
(use-package! vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")
;; Vim:2 ends here

;; [[file:config.org::*GNU Octave][GNU Octave:1]]
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; GNU Octave:1 ends here

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
        (:when (featurep! :tools debugger +lsp)
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

;; [[file:config.org::*DAP][DAP:2]]
(after! dap-mode
  (require 'dap-cpptools)

  ;; More minimal UI
  (setq dap-auto-configure-features '(locals tooltip)
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
      (:when (featurep! :tools debugger +lsp)
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
        (:when (featurep! :tools debugger)
         :prefix ("d" . "debugger")
         :desc "RealGUD hydra" "h" #'+debugger/realgud:gdb-hydra)))
;; Additional commands:1 ends here

;; [[file:config.org::*RealGUD =.dir-locals.el= support][RealGUD =.dir-locals.el= support:1]]
;; A variable which to be used in .dir-locals.el, formatted as a property list;
;; '(:program "..." :args ("args1" "arg2" ...))
;; "${workspaceFolder}" => gets replaced with project workspace (from projectile)
;; "${workspaceFolderBasename}" => gets replaced with project workspace's basename
(defvar +realgud:launch-plist nil)
;; RealGUD =.dir-locals.el= support:1 ends here

;; [[file:config.org::*RealGUD =.dir-locals.el= support][RealGUD =.dir-locals.el= support:3]]
(defun +realgud:get-launch-debugger-args (&key program args)
  (let ((debugger--args ""))
    (when program
      (setq debugger--args program)
      (when args
        (setq debugger--args (concat debugger--args " " (s-join " " args)))))
    ;; Replace special variables
    (let* ((ws--root (expand-file-name (or (projectile-project-root) ".")))
           (ws--basename (file-name-nondirectory
                          (if (s-ends-with-p "/" ws--root)
                              (substring ws--root 0 -1)
                            ws--root))))
      (s-replace-all
       (list (cons "${workspaceFolder}" ws--root)
             (cons "${workspaceFolderBasename}" ws--basename))
       debugger--args))))

(defun +debugger/realgud:gdb-launch ()
  "Launch RealGUD with parameters from `+realgud:launch-plist'"
  (interactive)
  (require 'realgud)
  (if +realgud:launch-plist
      (realgud:gdb
       (concat realgud:gdb-command-name
               " --args "
               (apply '+realgud:get-launch-debugger-args +realgud:launch-plist)))
    (progn
      (message "Variable `+realgud:launch-plist' is `nil'")
      (realgud:gdb))))

(map! :leader :prefix ("l" . "custom")
      (:when (featurep! :tools debugger)
       :prefix ("d" . "debugger")
       :desc "RealGUD launch" "d" #'+debugger/realgud:gdb-launch))
;; RealGUD =.dir-locals.el= support:3 ends here

;; [[file:config.org::*Record and replay =rr=][Record and replay =rr=:1]]
(after! realgud
  (require 's)

  (defun +debugger/rr-replay ()
    "Launch `rr replay'"
    (interactive)
    (realgud:gdb (s-replace "gdb" "rr replay" realgud:gdb-command-name)))

  (defun +debugger/rr-record ()
    "Launch `rr record' with parameters from `+realgud:launch-plist'"
    (interactive)
    (let ((debugger--args (apply '+realgud:get-launch-debugger-args +realgud:launch-plist)))
      (unless (make-process :name "*rr record*"
                            :buffer "*rr record*"
                            :command (append '("rr" "record") (s-split " " debugger--args)))
        (message "Cannot make process 'rr record'"))))

  (map! :leader :prefix ("l" . "custom")
        (:when (featurep! :tools debugger)
         :prefix ("d" . "debugger")
         :desc "rr record" "r" #'+debugger/rr-record
         :desc "rr replay" "R" #'+debugger/rr-replay)))
;; Record and replay =rr=:1 ends here

;; [[file:config.org::*Emacs GDB][Emacs GDB:2]]
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
;; Emacs GDB:2 ends here

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

;; [[file:config.org::*Valgrind][Valgrind:2]]
(use-package! valgrind
  :commands valgrind)
;; Valgrind:2 ends here

;; [[file:config.org::*Repo][Repo:2]]
(use-package! repo
  :when REPO-P
  :commands repo-status)
;; Repo:2 ends here

;; [[file:config.org::*Blamer][Blamer:2]]
(use-package! blamer
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

  :hook ((prog-mode . blamer-mode)
         (text-mode . blamer-mode))

  :config
  (when (featurep! :ui zen) ;; Disable in zen (writeroom) mode
    (add-hook! 'writeroom-mode-enable-hook (blamer-mode -1))
    (add-hook! 'writeroom-mode-disable-hook (blamer-mode 1))))
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
  (when (featurep! :tools pdf)
    (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools))
  ;; Get manual from https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (setq x86-lookup-pdf (expand-file-name "x86-lookup/325383-sdm-vol-2abcd.pdf" doom-etc-dir)))
;; Assembly:2 ends here

;; [[file:config.org::*Disaster][Disaster:2]]
(use-package! disaster
  :commands (disaster)
  :init
  (setq disaster-assembly-mode 'nasm-mode)

  (map! :localleader
        :map (c++-mode-map c-mode-map)
        :desc "Disaster" "d" #'disaster))
;; Disaster:2 ends here

;; [[file:config.org::*Devdocs][Devdocs:2]]
(use-package! devdocs
  :commands (devdocs-lookup devdocs-install)
  :config
  (setq devdocs-data-dir (expand-file-name "devdocs" doom-etc-dir)))
;; Devdocs:2 ends here

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

;; [[file:config.org::*Mermaid][Mermaid:2]]
(use-package! mermaid-mode
  :commands mermaid-mode
  :mode ("\\.mmd\\'"))

(use-package! ob-mermaid
  :after org
  :init
  (after! org
    (add-to-list 'org-babel-load-languages '(mermaid . t))))
;; Mermaid:2 ends here

;; [[file:config.org::*Inspector][Inspector:2]]
(use-package! inspector
  :commands (inspect-expression inspect-last-sexp))
;; Inspector:2 ends here

(after! org
  (setq org-directory "~/Dropbox/Org/"        ; let's put files here
        org-use-property-inheritance t        ; it's convenient to have properties inherited
        org-log-done 'time                    ; having the time an item is done sounds convenient
        org-list-allow-alphabetical t         ; have a. A. a) A) list bullets
  ;;    org-export-in-background t            ; run export processes in external emacs process
  ;;    org-export-async-debug t
        org-tags-column 0
        org-catch-invisible-edits 'smart      ;; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{}  ;; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
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
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)
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
  
  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("home" . ?h)
          ("research" . ?r)
          ("work" . ?w)
          (:endgroup . nil)
          (:startgroup . nil)
          ("tool" . ?o)
          ("dev" . ?d)
          ("report" . ?p)
          (:endgroup . nil)
          (:startgroup . nil)
          ("easy" . ?e)
          ("medium" . ?m)
          ("hard" . ?a)
          (:endgroup . nil)
          ("urgent" . ?u)
          ("key" . ?k)
          ("bonus" . ?b)
          ("noexport" . ?x)))
  
  (setq org-tag-faces
        '(("home" . (:foreground "goldenrod" :weight bold))
          ("research" . (:foreground "goldenrod" :weight bold))
          ("work" . (:foreground "goldenrod" :weight bold))
          ("tool" . (:foreground "IndianRed1" :weight bold))
          ("dev" . (:foreground "IndianRed1" :weight bold))
          ("report" . (:foreground "IndianRed1" :weight bold))
          ("urgent" . (:foreground "red" :weight bold))
          ("key" . (:foreground "red" :weight bold))
          ("easy" . (:foreground "green4" :weight bold))
          ("medium" . (:foreground "orange" :weight bold))
          ("hard" . (:foreground "red" :weight bold))
          ("bonus" . (:foreground "goldenrod" :weight bold))
          ("noexport" . (:foreground "LimeGreen" :weight bold))))
  
  ;; (defun log-todo-next-creation-date (&rest ignore)
  ;;   "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  ;;   (when (and (string= (org-get-todo-state) "NEXT")
  ;;              (not (org-entry-get nil "ACTIVATED")))
  ;;     (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  
  ;; (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
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
        (setq doct-templates (mapcar (lambda (template)
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
                                 :extra ""
                                 )
                                ("Task with deadline" :keys "d"
                                 :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                 :extra "\nDEADLINE: %^{Deadline:}t"
                                 )
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
          org-modern-table-vertical 1
          org-modern-table-horizontal 1
          org-modern-list '((43 . "➤")
                            (45 . "–")
                            (42 . "•"))
          org-modern-footnote (cons nil (cadr org-script-display))
          org-modern-priority nil
          org-modern-horizontal-rule t
          org-modern-todo-faces
          '(("TODO" :inverse-video t :inherit org-todo)
            ("PROJ" :inverse-video t :inherit +org-todo-project)
            ("STRT" :inverse-video t :inherit +org-todo-active)
            ("[-]"  :inverse-video t :inherit +org-todo-active)
            ("HOLD" :inverse-video t :inherit +org-todo-onhold)
            ("WAIT" :inverse-video t :inherit +org-todo-onhold)
            ("[?]"  :inverse-video t :inherit +org-todo-onhold)
            ("KILL" :inverse-video t :inherit +org-todo-cancel)
            ("NO"   :inverse-video t :inherit +org-todo-cancel))
          org-modern-keyword
          '((t . t)
            ("title" . "𝙏")
            ("subtitle" . "𝙩")
            ("author" . "𝘼")
            ("email" . "@")
            ("date" . "𝘿")
            ("property" . "☸")
            ("options" . "⌥")
            ("startup" . "⏻")
            ("macro" . "𝓜")
            ("bind" . #("" 0 1 (display (raise -0.1))))
            ("bibliography" . "")
            ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
            ("cite_export" . "⮭")
            ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
            ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
            ("export_file_name" . "⇒")
            ("include" . "⇤")
            ("setupfile" . "⇐")
            ("html_head" . "🅷")
            ("html" . "🅗")
            ("latex_class" . "🄻")
            ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
            ("latex_header" . "🅻")
            ("latex_header_extra" . "🅻⁺")
            ("latex" . "🅛")
            ("beamer_theme" . "🄱")
            ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
            ("beamer_font_theme" . "🄱𝐀")
            ("beamer_header" . "🅱")
            ("beamer" . "🅑")
            ("attr_latex" . "🄛")
            ("attr_html" . "🄗")
            ("attr_org" . "⒪")
            ("name" . "⁍")
            ("header" . "›")
            ("caption" . "☰")
            ("RESULTS" . "🠶")
            ("language" . "𝙇")
            ("hugo_base_dir" . "𝐇")
            ("latex_compiler" . "⟾")
            ("results" . "🠶")
            ("filetags" . "#")
            ("created" . "⏱")
            ("export_select_tags" . "✔")
            ("export_exclude_tags" . "❌")))
    (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))
  (defadvice! +org-init-appearance-h--no-ligatures-a ()
    :after #'+org-init-appearance-h
    (set-ligatures! 'org-mode
      :name nil
      :src_block nil
      :src_block_end nil
      :quote nil
      :quote_end nil))
  (use-package! org-ol-tree
    :commands org-ol-tree
    :config
    (setq org-ol-tree-ui-icon-set
          (if (and (display-graphic-p)
                   (fboundp 'all-the-icons-material))
              'all-the-icons
            'unicode))
    (org-ol-tree-ui--update-icon-set))
  
  (map! :map org-mode-map
        :after org
        :localleader
        :desc "Outline" "O" #'org-ol-tree)
  ;; From https://www.reddit.com/r/orgmode/comments/i6hl8b/comment/g1vsef2/
  ;; Scale image previews to 60% of the window width.
  (setq org-image-actual-width (truncate (* (window-pixel-width) 0.6)))
  (setq org-list-demote-modify-bullet
        '(("+" . "-")
          ("-" . "+")
          ("*" . "+")
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
  ;; (setq org-format-latex-header "\\documentclass{article}
  ;; \\usepackage[svgnames]{xcolor}
  ;; \\usepackage[T1]{fontenc}
  ;; \\usepackage{booktabs}
  
  ;; \\pagestyle{empty} % do not remove
  
  ;; \\setlength{\\textwidth}{\\paperwidth}
  ;; \\addtolength{\\textwidth}{-3cm}
  ;; \\setlength{\\oddsidemargin}{1.5cm}
  ;; \\addtolength{\\oddsidemargin}{-2.54cm}
  ;; \\setlength{\\evensidemargin}{\\oddsidemargin}
  ;; \\setlength{\\textheight}{\\paperheight}
  ;; \\addtolength{\\textheight}{-\\headheight}
  ;; \\addtolength{\\textheight}{-\\headsep}
  ;; \\addtolength{\\textheight}{-\\footskip}
  ;; \\addtolength{\\textheight}{-3cm}
  ;; \\setlength{\\topmargin}{1.5cm}
  ;; \\addtolength{\\topmargin}{-2.54cm}
  ;; \\usepackage{arev}
  ;; ")
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
  (when (featurep! :ui zen)
    (add-hook! 'writeroom-mode-enable-hook (+org-format-latex-set-scale 2.0))
    (add-hook! 'writeroom-mode-disable-hook (+org-format-latex-set-scale 1.4)))
  (defun +scimax-org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
          (counter -1)
          (numberp))
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
                       (cons begin counter)
                       (message "Entered equation env, counter=%d" counter))
                      ((string-match "\\\\begin{align}" env)
                       (prog2
                           (cl-incf counter)
                           (cons begin counter)
                         (with-temp-buffer
                           (insert env)
                           (goto-char (point-min))
                           ;; \\ is used for a new line. Each one leads to a number
                           (cl-incf counter (count-matches "\\\\$"))
                           ;; unless there are nonumbers.
                           (goto-char (point-min))
                           (cl-decf counter (count-matches "\\nonumber")))))
                      (t
                       (cons begin nil)))))
  
      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))
  
    (apply orig-func args))
  
  
  (defun +scimax-toggle-latex-equation-numbering ()
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (not (get '+scimax-org-renumber-environment 'enabled))
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
  (use-package! org-fragtog
    :hook (org-mode . org-fragtog-mode))
  (after! org-plot
    (defun org-plot/generate-theme (_type)
      "Use the current Doom theme colours to generate a GnuPlot preamble."
      (format "
  fgt = \"textcolor rgb '%s'\" # foreground text
  fgat = \"textcolor rgb '%s'\" # foreground alt text
  fgl = \"linecolor rgb '%s'\" # foreground line
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
              (doom-color 'violet)
              ))
    (defun org-plot/gnuplot-term-properties (_type)
      (format "background rgb '%s' size 1050,650"
              (doom-color 'bg)))
    (setq org-plot/gnuplot-script-preamble #'org-plot/generate-theme)
    (setq org-plot/gnuplot-term-extra #'org-plot/gnuplot-term-properties))
  (setq bibtex-completion-bibliography '("~/Zotero/library.bib")
        bibtex-completion-library-path '("~/Zotero/storage/")
        bibtex-completion-notes-path "~/PhD/bibliography/notes/"
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
    (setq org-cite-csl-styles-dir "~/Zotero/styles")
  
    (defun org-ref-to-org-cite ()
      "Attempt to convert org-ref citations to org-cite syntax."
      (interactive)
      (let* ((cite-conversions '(("cite" . "//b") ("Cite" . "//bc")
                                 ("nocite" . "/n")
                                 ("citep" . "") ("citep*" . "//f")
                                 ("parencite" . "") ("Parencite" . "//c")
                                 ("citeauthor" . "/a/f") ("citeauthor*" . "/a")
                                 ("citeyear" . "/na/b")
                                 ("Citep" . "//c") ("Citealp" . "//bc")
                                 ("Citeauthor" . "/a/cf") ("Citeauthor*" . "/a/c")
                                 ("autocite" . "") ("Autocite" . "//c")
                                 ("notecite" . "/l/b") ("Notecite" . "/l/bc")
                                 ("pnotecite" . "/l") ("Pnotecite" . "/l/bc")))
             (cite-regexp (rx (regexp (regexp-opt (mapcar #'car cite-conversions) t))
                              ":" (group (+ (not (any "\n     ,.)]}")))))))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward cite-regexp nil t)
            (message (format "[cite%s:@%s]"
                             (cdr (assoc (match-string 1) cite-conversions))
                             (match-string 2)))
            (replace-match (format "[cite%s:@%s]"
                                   (cdr (assoc (match-string 1) cite-conversions))
                                   (match-string 2))))))))
  (use-package! org-ref
    :after org
    :config
    (defadvice! org-ref-open-bibtex-pdf-a ()
      :override #'org-ref-open-bibtex-pdf
      (save-excursion
        (bibtex-beginning-of-entry)
        (let* ((bibtex-expand-strings t)
               (entry (bibtex-parse-entry t))
               (key (reftex-get-bib-field "=key=" entry))
               (pdf (or
                     (car (-filter (lambda (f) (string-match-p "\\.pdf$" f))
                                   (split-string (reftex-get-bib-field "file" entry) ";")))
                     (funcall 'org-ref-get-pdf-filename key))))
          (if (file-exists-p pdf)
              (org-open-file pdf)
            (ding)))))
  
    (defadvice! org-ref-open-pdf-at-point-a ()
      "Open the pdf for bibtex key under point if it exists."
      :override #'org-ref-open-pdf-at-point
      (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (funcall 'org-ref-get-pdf-filename key)))
        (with-current-buffer (find-file-noselect (cdr results))
          (save-excursion
            (bibtex-search-entry (car results))
            (org-ref-open-bibtex-pdf)))))
  
    ;; Add keybinding to insert link
    (map! :localleader
          :map org-mode-map
          :desc "Org-ref insert link" "C" #'org-ref-insert-link))
  (setq citar-library-paths '("~/Zotero/storage")
        citar-notes-paths '("~/PhD/bibliography/notes/")
        citar-bibliography '("~/Zotero/library.bib"))
  (setq org-export-headline-levels 5) ;; I like nesting
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-creator-string
        (format "Emacs %s (Org mode %s)" emacs-version (org-release)))
  ;; `org-latex-compilers' contains a list of possible values ("pdflatex" "xelatex" "lualatex")
  ;; for the `%latex' argument.
  (setq org-latex-pdf-process '("latexmk -shell-escape -pdf -quiet -f -%latex -interaction=nonstopmode -output-directory=%o %f"))
  ;; Add 'svg' package to display SVG pictures (uses inkscape, imagemagik and ghostscript)
  ;; (add-to-list 'org-latex-packages-alist '("" "svg"))
  ;; (add-to-list 'org-latex-packages-alist '("" "fontspec")) ;; for xelatex
  ;; (add-to-list 'org-latex-packages-alist '("utf8" "inputenc"))
  ;; this is for code syntax highlighting in export. you need to use
  ;; -shell-escape with latex, and install pygments.
  ;; (add-to-list 'org-latex-packages-alist '("svgnames" "xcolor"))
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  
  ;; (setq org-latex-listings 'minted) ;; Per document, in local variables
  (setq org-latex-minted-options '(("frame" "lines")
                                   ("fontsize" "\\footnotesize")
                                   ("tabsize" "2")
                                   ("breaklines" "")
                                   ("breakanywhere" "") ;; break anywhere, no just on spaces
                                   ("style" "default")
                                   ("bgcolor" "GhostWhite")
                                   ("linenos" "")))
  
  (dolist (pair '((ipython    "python")
                  (jupyter    "python")
                  (scheme     "scheme")
                  (lisp-data  "lisp")
                  (conf       "ini")
                  (conf-unix  "unixconfig")
                  (conf-space "unixconfig")
                  (conf-toml  "yaml")
                  (gitconfig  "ini")
                  (systemd    "ini")
                  (gdb-script "text")))
    (unless (member pair org-latex-minted-langs)
      (add-to-list 'org-latex-minted-langs pair)))
  (after! ox-latex
    (add-to-list 'org-latex-classes
                 '("scr-article"
                   "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("lettre"
                   "\\documentclass{lettre}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("blank"
                   "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("bmc-article"
                   "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("bmc"
                   "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("IEEEtran"
                   "\\documentclass{IEEEtran}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("ieeeconf"
                   "\\documentclass{ieeeconf}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("sagej"
                   "\\documentclass{sagej}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("thesis"
                   "\\documentclass[11pt]{book}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("thesis-fr"
                   "\\documentclass[french,12pt,a4paper]{book}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}"))))
  
  (setq org-latex-default-class "article")
  ;; org-latex-tables-booktabs t
  ;; org-latex-reference-command "\\cref{%s}")
  (advice-add 'org-latex-export-to-pdf :around
              (lambda (orig-fn &rest orig-args)
                (message "Export to PDF %s."
                         (if (file-exists-p (expand-file-name "main.org"))
                             (with-current-buffer (find-file-noselect "main.org")
                               (apply orig-fn orig-args))
                           (apply orig-fn orig-args))
                         "succeeded" "failed")))
  (setq time-stamp-active t
        time-stamp-start "#\\+lastmod:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "%04Y-%02m-%02d")
  
  (add-hook 'before-save-hook 'time-stamp nil)
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
