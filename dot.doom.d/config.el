(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@cryptolab.net")

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq fancy-splash-image "~/.doom.d/blackhole.png")

(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 30)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 30))

;; Activate solaire mode, this HAS to be called before loading the theme
(solaire-global-mode +1)

(setq doom-theme 'doom-one) ; Load theme
;; (setq doom-theme 'doom-palenight) ; Load theme
;; (setq doom-theme 'doom-horizon)   ; Load theme
;; (setq doom-theme 'doom-snazzy) ; Load theme

(setq display-line-numbers-type t) ; Enable line numbers

(require 'evil-numbers)

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; This will set the time format to 24h
(setq display-time-string-forms
      '((propertize (concat 24-hours ":" minutes))))

(display-time-mode) ;; Display the time
(display-battery-mode) ;; Display the battery status

(setq org-directory "~/Work/org/")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t))) ; this line activates ditaa

(defun ab-conf/set-bidi-env ()
  "interactive"
  (setq bidi-paragraph-direction 'nil))
(add-hook 'org-mode-hook 'ab-conf/set-bidi-env)

(require 'ox-moderncv)

(setq org-agenda-files (list "~/Work/org/inbox.org" "~/Work/org/agenda.org"
                             "~/Work/org/notes.org" "~/Work/org/projects.org"))
;(setq org-agenda-files (list "~/Work/org"))

(setq org-capture-templates
      `(("i" "Inbox" entry (file "inbox.org")
         "* TODO %?\n/Entered on/ %U")
        ("m" "Meeting" entry (file+headline "agenda.org" "Future")
         "* %? :meeting:\n<%<%Y-%m-%d %a %H:00>>")
        ("n" "Note" entry (file "notes.org")
         "* Note (%a)\n/Entered on/ %U\n" "\n" "%?")
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
         "* TODO Reply to \"%a\" %?\n/Entered on/ %U")))


(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(defun org-capture-mail ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Use full window for org-capture
;(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Key bindings
(define-key global-map            (kbd "C-c a") 'org-agenda)
(define-key global-map            (kbd "C-c c") 'org-capture)
(define-key global-map            (kbd "C-c i") 'org-capture-inbox)

;; Only if you use mu4e
(require 'mu4e)
(define-key mu4e-headers-mode-map (kbd "C-c i") 'org-capture-mail)
(define-key mu4e-view-mode-map    (kbd "C-c i") 'org-capture-mail)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")))
(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

(custom-set-variables ; in ~/.emacs, only one instance
 '(org-export-latex-classes (quote ; in the init file!
    (("beamer" "\\documentclass{beamer}"
        org-beamer-sectioning))))
 '(org-latex-to-pdf-process (quote
    ((concat "pdflatex -interaction nonstopmode"
             "-shell-escape -output-directory %o %f")
     "bibtex $(basename %b)"
     (concat "pdflatex -interaction nonstopmode"
             "-shell-escape -output-directory %o %f")
     (concat "pdflatex -interaction nonstopmode"
             "-shell-escape -output-directory %o %f")))))

(defun ab-conf/prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'ab-conf/prefer-horizontal-split)

(defun ab-conf/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . ab-conf/org-mode-visual-fill))

(dolist (hook '(text-mode-hook markdow-mode-hook tex-mode-hook magit-mode-hook))
  (add-hook hook 'ab-conf/org-mode-visual-fill))

(map!
  (:after dired
    (:map dired-mode-map
     "C-x i" #'peep-dired
     )))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(use-package emms
  :config
  (require 'emms-setup)
  (require 'emms-info)
  (require 'emms-cue)
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (setq emms-source-file-default-directory "~/Music/Mohamed Rouane - Nulle Part/")
  (setq emms-playlist-buffer-name "*EMMS Playlist*")
  (setq emms-info-asynchronously t)
  (unless (eq system-type 'windows-nt)
    (setq emms-source-file-directory-tree-function
          'emms-source-file-directory-tree-find))
  (emms-all)
  (emms-default-players)
  (emms-mode-line 1)
  (emms-playing-time 1))

(map! :leader
      :desc "Open EMMS" "o M" #'emms)

(map! :leader
      :desc "Open serial port terminal" "o s" #'serial-term)

(use-package emacs-rg
 :requires 'rg)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)
(setq user-mail-address "user@example.com"
      user-full-name  "Abdelhak Bougouffa"
      mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
      mu4e-update-interval 300
      mu4e-compose-signature
      (concat "- Abdelhak BOUGOUFFA\n"
              "- Doctorant | Ingénieur R&D\n"
              "- Université Paris-Saclay - SATIE | ez-Wheel\n")
      mu4e-main-buffer-hide-personal-addresses t
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-smtp-service 587
      smtpmail-smtp-server "smtp.example.com"
      ;; smtpmail-starttls-credentials (expand-file-name "~/.config/mu4e/authinfo.gpg")
      smtpmail-auth-credentials (expand-file-name "~/.config/mu4e/authinfo.gpg")
      mu4e-sent-folder "/account/Sent Items"
      mu4e-drafts-folder "/account/Drafts"
      mu4e-trash-folder "/account/Trash"
      mu4e-maildir-shortcuts
      '(("/account0/INBOX" . ?i)
        ("/account/INBOX"          . ?I)
        ("/account/Sent Items"     . ?s)
        ("/account/Drafts"         . ?d)
        ("/account/Trash"          . ?t)))

(use-package racer
  :requires rust-mode

  :init (setq racer-rust-src-path
              (concat (string-trim
                       (shell-command-to-string "rustc --print sysroot"))
                      "/lib/rustlib/src/rust/src"))

  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))
