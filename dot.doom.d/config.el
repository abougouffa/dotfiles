(setq user-full-name "Abdelhak Bougouffa"
      user-mail-address "abougouffa@cryptolab.net")

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

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)

(setq browse-url-browser-function 'eww-browse-url)

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
