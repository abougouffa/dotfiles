;;; emacs/dired-ng/config.el -*- lexical-binding: t; -*-

(use-package! dired
  :commands dired-jump
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)

  (let ((non-gnu-args "-alh")
        (gnu-args
         "-l --almost-all --human-readable --group-directories-first"))
    (cond ((and (boundp 'ls-lisp-use-insert-directory-program)
                (not ls-lisp-use-insert-directory-program)
                (< emacs-major-version 28))
           ;; Fixes #3939: unsortable dired entries on Windows
           (setq dired-listing-switches non-gnu-args))
          (IS-BSD
           ;; BSD ls doesn't support long options
           (if-let (gls (executable-find "gls"))
               (setq insert-directory-program gls
                     dired-listing-switches gnu-args)
             (setq dired-listing-switches non-gnu-args)))
          (t (setq dired-listing-switches gnu-args))))

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))

  (map! :map dired-mode-map
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  (map! :map dired-mode-map
        :localleader
        "h" #'dired-omit-mode))


(use-package! dired-aux
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode))


(use-package! dirvish
  :defer t
  :general (dired-mode-map "C-c C-r" #'dirvish-yank) ; for backward compatibility
  :after-call dired-noselect dirvish-fd
  :config
  (dirvish-override-dired-mode)
  (set-popup-rule! "^ ?\\*Dirvish.*" :ignore t)
  (map! :map dirvish-mode-map
        :n "q"    #'dirvish-quit
        :n "?"    #'dirvish-dispatch
        :n "M-t"  #'dirvish-layout-toggle)
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-mode-line-format
        '(:left (sort file-time symlink) :right (omit yank index)))
  (when (modulep! :ui tabs)
    (after! centaur-tabs
      (add-hook! 'dirvish-mode-hook 'centaur-tabs-local-mode)
      (add-hook! 'dirvish-parent-mode-hook 'centaur-tabs-local-mode)))
  (when (modulep! :ui vc-gutter)
    (push 'vc-state dirvish-attributes))
  (when (modulep! +icons)
    (appendq! dirvish-attributes '(all-the-icons subtree-state)))
  (when (modulep! +bindings)
    (map! :map dirvish-mode-map
          :n "a"    #'dirvish-quick-access
          :n "f"    #'dirvish-file-info-menu
          :n "y"    #'dirvish-yank-menu
          :n "s"    #'dirvish-quicksort
          :n "TAB"  #'dirvish-subtree-toggle
          :n "M-f"  #'dirvish-history-go-forward
          :n "M-b"  #'dirvish-history-go-backward
          :n "M-n"  #'dirvish-narrow
          :n "M-m"  #'dirvish-mark-menu
          :n "M-s"  #'dirvish-setup-menu
          :n "M-e"  #'dirvish-emerge-menu
          :n "M-j"  #'dirvish-fd-jump)))
