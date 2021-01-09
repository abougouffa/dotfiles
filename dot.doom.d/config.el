(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@cryptolab.net")

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq fancy-splash-image "~/.doom.d/nasa_blackhole.png")

(setq doom-font (font-spec :family "JetBrains Mono" :size 30)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 30))

;; Activate solaire-mode, this have to be called before loading the theme
(solaire-global-mode +1)

;; (setq doom-theme 'doom-one) ; Load theme
;; (setq doom-theme 'doom-palenight) ; Load theme
;; (setq doom-theme 'doom-horizon)   ; Load theme
(setq doom-theme 'doom-old-hope)   ; Load theme
;; (setq doom-theme 'doom-snazzy) ; Load theme
;; (setq doom-theme 'doom-peacock) ; Load theme

(setq display-line-numbers-type t) ; Enable line numbers

(require 'evil-numbers)

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; This will set the time format to 24h
(setq display-time-string-forms
      '((propertize (concat 24-hours ":" minutes))))

(display-time-mode) ;; Display the time
(display-battery-mode) ;; Display the battery status

(defun ab-conf/spelldict (lang)
  "Switch between language dictionaries."
  (interactive)
  (cond ((eq lang 1)
         (setq flyspell-default-dictionary "american")
         (setq ispell-dictionary "american")
         (ispell-kill-ispell)
         (spell-fu-mode)
         (spell-fu-mode)
         (message "Dictionary changed to 'american'"))
        ((eq lang 2)
         (setq flyspell-default-dictionary "francais")
         (setq ispell-dictionary "francais")
         (ispell-kill-ispell)
         (spell-fu-mode)
         (spell-fu-mode)
         (message "Dictionary changed to 'francais'"))
        (t (message "No changes have been made."))))

(map! :leader
      :desc "Spell dictionary" "t d")

(map! :leader
      :desc "American" "t d a" #'(lambda () (interactive) (ab-conf/spelldict 1)))

(map! :leader
      :desc "Français" "t d f" #'(lambda () (interactive) (ab-conf/spelldict 2)))

(setq org-directory "~/Work/org/")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (shell . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ocaml . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (screen . t)
   (plantuml . t)
   (lilypond . t)
   (org . t)
   (ditaa . t)
   (makefile . t)
   ))

;; (setq org-src-preserve-indentation t)

(setq ab-conf/new-org-templates (version<= "9.2" (org-version)))
(when ab-conf/new-org-templates
  (require 'org-tempo))

(defun ab-conf/add-org-template (old-style-template)
  (add-to-list 'org-structure-template-alist
               (if ab-conf/new-org-templates ; change the template format for Org Mode >= 9.8
                   (cons
                    (car old-style-template)
                    ;; Take the second element and trim the #+begin_ and #+end_src
                    ;; to fit the new template style
                    ;; For example,
                    ;; ("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>")
                    ;; becomes
                    ;; ("m" "src emacs-lisp\n\n" "<src lang=\"emacs-lisp\">\n\n</src>")
                    (string-trim-right
                     (substring (car (cdr old-style-template)) 8 -9)))
                 old-style-template)))

(unless ab-conf/new-org-templates
  ;; this template is predefined in the new templating system
  (ab-conf/add-org-template
   '("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")))

;; Emacs-lisp
(ab-conf/add-org-template
 '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

;; R
(ab-conf/add-org-template
 '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

;; R, this creates an R block for graphics
;; that are stored in the =/tmp/=.
(ab-conf/add-org-template
 '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

;; R, this creates an R block for
;; graphics that are stored in the directory of the current file.
(ab-conf/add-org-template
 '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

;; Python
(ab-conf/add-org-template
 '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(ab-conf/add-org-template
 '("P" "#+begin_src python :results output :session *py* :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(ab-conf/add-org-template
 '("PP" "#+begin_src python :results file :session *py* :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename\n#+end_src" "<src lang=\"python\">\n\n</src>"))

;; Bash Shell
(if (memq system-type '(windows-nt ms-dos))
    ;; Non-session shell execution does not seem to work under Windows, so we use
    ;; a named session just like for B.
    (ab-conf/add-org-template
     '("b" "#+begin_src shell :session session :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))
  (ab-conf/add-org-template
   '("b" "#+begin_src shell :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>")))

;; Bash Shell, this comes with a session argument (e.g., in case you want to keep ssh connexions open).
(ab-conf/add-org-template
 '("B" "#+begin_src shell :session *shell* :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

;; Graphviz
(ab-conf/add-org-template
 '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
digraph G {
node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
A[label=\"A\"]
B[label=\"B\"]
A->B
}\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

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

(dolist (hook '(text-mode-hook markdow-mode-hook tex-mode-hook magit-mode-hook repo-mode-hook))
  (add-hook hook 'ab-conf/org-mode-visual-fill))

(map!
  (:after dired
    (:map dired-mode-map
     "C-x i" #'peep-dired
     )))
(evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      :desc "Go to emms playlist"
      "a a" #'emms-playlist-mode-go
      :leader
      :desc "Emms pause track"
      "a x" #'emms-pause
      :leader
      :desc "Emms stop track"
      "a s" #'emms-stop
      :leader
      :desc "Emms play previous track"
      "a p" #'emms-previous
      :leader
      :desc "Emms play next track"
      "a n" #'emms-next)

(map! :leader
      :desc "Open serial port terminal" "o s" #'serial-term)

(use-package emacs-rg
 :requires 'rg)

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

(require 'bitbake)
(setq auto-mode-alist (cons '("\\.bb$" . bitbake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . bitbake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbappend$" . bitbake-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbclass$" . bitbake-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.launch$" . xml-mode) auto-mode-alist))
