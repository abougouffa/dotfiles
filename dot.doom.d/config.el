(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@cryptolab.net")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 27)
       doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font" :size 27))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type t) ; Enable line numbers

;; This will set the time format to 24h
(setq display-time-string-forms
      '((propertize (concat 24-hours ":" minutes))))

(display-time-mode) ;; This will display the time
(display-battery-mode) ;; This will display the battery status

(setq org-directory "~/Work/org/")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-agenda-files (list "~/Work/org/inbox.org" "~/Work/org/agenda.org"
                             "~/Work/org/notes.org" "~/Work/org/projects.org"))
;(setq org-agenda-files (list "~/Work/org"))

(setq org-capture-templates
      '(("i" "Inbox" entry (file "inbox.org")
         "* TODO %?\n/Entered on/ %U")
        ("m" "Meeting" entry (file+headline "agenda.org" "Future")
         "* %? :meeting:\n<%<%Y-%m-%d %a %H:00>>")
        ("n" "Note" entry (file "notes.org")
         "* Note (%a)\n/Entered on/ %U\n" "\n" "%?")
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
         "* TODO Reply to \"%a\" %?\n/Entered on/ %U")))

(setq org-capture-templates
      '(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
        ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
        ,(concat "* %? :meeting:\n"
                 "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry  (file "notes.org")
        ,(concat "* Note (%a)\n"
                 "/Entered on/ %U\n" "\n" "%?"))
        ("@" "Inbox [mu4e]" entry (file "inbox.org")
        ,(concat "* TODO Reply to \"%a\" %?\n"
                 "/Entered on/ %U"))))

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

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

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
