;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; * Chat
     erc slack

     ;; * Checkers
     syntax-checking spell-checking
     (languagetool :variables
                   langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"
                   langtool-default-language "en-US")

     ;; * Completion
     auto-completion helm templates

     ;; * Emacs
     helpful ibuffer tabs
     (org :variables
          org-enable-org-journal-support t
          org-journal-dir "~/Org/journal/"
          org-journal-file-format "%Y-%m-%d"

          org-enable-github-support t
          org-enable-sticky-header t

          org-enable-roam-support t
          ;; org-roam-db-location "~/Org/roam"
          org-roam-index-file "~/Org/roam/index.org"
          org-roam-directory "~/Org/roam")

     ;; * Email
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp"
           mu4e-use-maildirs-extension t
           mu4e-enable-notifications t
           mu4e-enable-mode-line t
           mu4e-spacemacs-layout-name "@Mu4e"
           mu4e-spacemacs-layout-binding "m"
           mu4e-spacemacs-kill-layout-on-exit t
           mu4e-org-compose-support t)

     ;; * File tree
     (treemacs :variables
               treemacs-use-all-the-icons-theme t)

     ;; * Fonts
     unicode-fonts

     ;; * Fun
     games

     ;; * Programming Languages
     ;; ** Domain-specific (DSLs)
     common-lisp emacs-lisp gpu

     ;; *** Markup & configuration
     asciidoc bibtex csv graphviz html json latex markdown plantuml restructuredtext yaml

     ;; *** Scripting
     shell-scripts vimscript windows-scripts

     ;; *** Extra Languages
     major-modes

     ;; *** Scientific
     ess julia octave ipython-notebook

     ;; ** General Purpose
     asm lua
     (python :variables
             python-backend 'lsp
             python-lsp-server 'pyright ;; yay -S pyright
             python-formatter 'yapf ;; pip install --user yapf
             python-format-on-save t)
     (rust :variables
           rust-format-on-save t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c-mode
            c-c++-adopt-subprojects t
            c-c++-backend 'lsp-clangd
            c-c++-lsp-enable-semantic-highlight 'rainbow
            c++-enable-organize-includes-on-save t
            c-c++-enable-clang-format-on-save t)

     ;; * Miscellaneous
     copy-as-format multiple-cursors

     ;; ** Spacemacs
     spacemacs-completion spacemacs-defaults spacemacs-editing spacemacs-editing-visual
     spacemacs-evil spacemacs-language spacemacs-layouts spacemacs-misc spacemacs-modeline
     spacemacs-navigation spacemacs-org spacemacs-project spacemacs-purpose spacemacs-visual

     ;; * Readers
     dash deft elfeed epub pdf speed-reading

     ;; * Source-Control
     git version-control

     ;; * Themes
     colors ; themes-megapack

     ;; * Tools
     ;; Dependencies:
     ;; pip install --user cmake-language-server
     dap debug docker pandoc prettier ranger sphinx systemd vagrant web-beautify xclipboard lsp
     (cmake :variables
            cmake-enable-cmake-ide-support t
            cmake-backend 'lsp)
     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 40
            shell-default-position 'bottom)

     ;; * Web Services
     confluence search-engine
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '( bitbake
                                       sqlite3 )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator bar :separator-scale 0.75)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Jetbrains Mono"
                               :size 12.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Display time in mode-line
  (setq display-time-string-forms
        '((propertize (concat 24-hours "h" minutes))))

  (display-time-mode)

  ;; Enable line wrapping globally
  (global-visual-line-mode t)

  ;; Enable syntax highlighting in exported PDFs
  (require 'org)
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-src-fontify-natively t)

  ;; Add margins to buffer in Org Mode,
  ;; change the =visual-fill-column-width= to the desired width of
  ;; the actual window (in letters, and counting the line number).
  (defun ab-conf/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :defer t
    :hook (org-mode . ab-conf/org-mode-visual-fill))

  ;; Add the same hook to other modes
  ;; (=text-mode=, =markdown-mode=, =tex-mode-hook=, =repo-mode= and =magit-mode=),
  ;; you can set additional modes in the list below:
  (dolist (hook '(text-mode-hook markdow-mode-hook tex-mode-hook magit-mode-hook repo-mode-hook))
    (add-hook hook 'ab-conf/org-mode-visual-fill))

  ;; Literate programming (=org-babel=) :babel:literate:
  ;; -> Add to languages
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

  ;; -> Adding templates
  ;; --> Enable the new template system for Org Mode 9.2 and later
  (setq ab-conf/new-org-templates t) ;;; (version<= "9.2" (org-version))
  (when ab-conf/new-org-templates
    (require 'org-tempo))

  ;; --> Template definitions for old and new template systems
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

  ;; --> Add templates
  ;; To use this type the prefix (like =<s=) and then =TAB=

  ;; | Prefix | Language                                        |
  ;; |--------+-------------------------------------------------|
  ;; | =<s=   | Generic (=#src= block)                          |
  ;; |--------+-------------------------------------------------|
  ;; | =<m=   | Emacs Lisp                                      |
  ;; |--------+-------------------------------------------------|
  ;; | =<r=   | R                                               |
  ;; | =<R=   | R + session + graphics                          |
  ;; | =<RR=  | Like =R=, with graphics stored with the project |
  ;; |--------+-------------------------------------------------|
  ;; | =<p=   | Python                                          |
  ;; | =<P=   | Python + session                                |
  ;; | =<PP=  | Python + session + graphics                     |
  ;; |--------+-------------------------------------------------|
  ;; | =<b=   | Bash shell                                      |
  ;; | =<B=   | Bash shell + session                            |
  ;; | =<bn=  | Bash (no output)                                |
  ;; |--------+-------------------------------------------------|
  ;; | =<g=   | Graphviz                                        |
  ;; |--------+-------------------------------------------------|

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

  ;; Bash Shell, this comes with a session argument (e.g., in case you want to keep ssh connexions open).
  (ab-conf/add-org-template
   '("bn" "#+begin_src shell \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

  ;; Graphviz
  (ab-conf/add-org-template
   '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
digraph G {
node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
A[label=\"A\"]
B[label=\"B\"]
A->B
}\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

  ;; ----------------

  ;; Email configuration (mu4e)
  ;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (setq mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbsyncrc -a"
        mu4e-update-interval 300
        mu4e-main-buffer-hide-personal-addresses t
        message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        ;; smtpmail-starttls-credentials (expand-file-name "~/.config/mu4e/authinfo-ezwheel.gpg")
        smtpmail-auth-credentials (expand-file-name "~/.config/mu4e/authinfo-ezwheel.gpg"))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "ez-Wheel"
             :enter-func (lambda () (mu4e-message "Switch to the ez-Wheel context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "a.bougouffa@ez-wheel.com")))
             :vars '( ( user-mail-address . "a.bougouffa@ez-wheel.com"  )
                      ( user-full-name . "Abdelhak Bougouffa" )
                      ( mu4e-compose-signature .
                                               (concat "- Abdelhak BOUGOUFFA\n"
                                                       "- Doctorant | Ingénieur R&D\n"
                                                       "- Université Paris-Saclay - SATIE | ez-Wheel\n"
                                                       "----------------\n"
                                                       "- abdelhak.bougouffa@universite-paris-saclay.fr\n"
                                                       "- a.bougouffa@ez-wheel.com\n"))

                      ( mu4e-main-buffer-hide-personal-addresses . t )
                      ( message-send-mail-function . 'smtpmail-send-it )
                      ( starttls-use-gnutls . t )
                      ( smtpmail-smtp-service . 587 )
                      ( smtpmail-smtp-server . "ex.mail.ovh.net" )
                      ( smtpmail-auth-credentials . (expand-file-name "~/.config/mu4e/authinfo-ezwheel.gpg") )
                      ))
           ,(make-mu4e-context
             :name "UP-Saclay"
             :enter-func (lambda () (mu4e-message "Switch to the University context"))
             ;; leave-fun not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "abdelhak.bougouffa@universite-paris-saclay.fr")))
             :vars '( ( user-mail-address . "abdelhak.bougouffa@universite-paris-saclay.fr" )
                      ( user-full-name . "Abdelhak Bougouffa" )
                      ( mu4e-compose-signature .
                                               (concat "- Abdelhak BOUGOUFFA\n"
                                                       "- Doctorant | Ingénieur R&D\n"
                                                       "- Université Paris-Saclay - SATIE | ez-Wheel\n"
                                                       "----------------\n"
                                                       "- abdelhak.bougouffa@universite-paris-saclay.fr\n"
                                                       "- a.bougouffa@ez-wheel.com\n"))
                      ( mu4e-main-buffer-hide-personal-addresses . t )
                      ( message-send-mail-function . 'smtpmail-send-it )
                      ( starttls-use-gnutls . t )
                      ( smtpmail-smtp-service . 587 )
                      ( smtpmail-smtp-server . "ex.mail.ovh.net" )
                      ( smtpmail-auth-credentials . (expand-file-name "~/.config/mu4e/authinfo-univ-paris-saclay.gpg"))))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; (setq mu4e-compose-context-policy nil)
  ;; * Org-Mode settings :org:
  ;; Set the default org-mode directory
  ;; ------------------------------
  (setq org-directory "~/Org/")

  ;; ** COMMENT RTL languages :rtl:arabic:
  ;; Enables [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Bidirectional-Editing.html][bidirectional editing]]

  ;; (defun ab-conf/set-bidi-env ()
  ;;   "interactive"
  ;;   (setq bidi-paragraph-direction 'nil))
  ;; (add-hook 'org-mode-hook 'ab-conf/set-bidi-env)

  ;; * GTD workflow :gtd:
  ;; Parts from this section has been taken form
  ;; [[https://www.labri.fr/perso/nrougier/GTD/index.html][Nicolas P. Rougier - Get Things Done with Emacs]] article.

  ;; ** Files and directories
  (setq org-agenda-files (list "~/Org/inbox.org" "~/Org/agenda.org"
                               "~/Org/notes.org" "~/Org/projects.org"))
                                        ;(setq org-agenda-files (list "~/Work/org"))

  ;; *** COMMENT Initial content of files
  ;; **** The =inbox.org= file:
  ;; #+STARTUP: content showstars indent
  ;; #+FILETAGS: inbox

  ;; The =STARTUP= line defines some buffer settings (initial visibility, indent mode and star visibility)
  ;; while the =FILETAGS= line define a common tag that will be inherited by all entries (=inbox= in this case).

  ;; **** The =agenda.org= file:
  ;; #+STARTUP: hideall showstars indent
  ;; #+TAGS:    event(e) meeting(m) deadline(d)
  ;; #+TAGS:    @outside(o) @company(p) @lab(b) @online(l) @canceled(c)

  ;; **** The =projects.org= file:
  ;; #+STARTUP: content showstars indent
  ;; #+TAGS: @home(h) @work(w) @mail(m) @comp(c) @web(b)
  ;; #+PROPERTY: Effort_ALL 0 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00
  ;; * Students :students:
  ;; * Team :team:
  ;; * Collaboratorive projects :collaborative:project:
  ;; * Events organization :events:
  ;; * Academic papers :article:
  ;; * Personal projects :personal:project:
  ;; * ez-Wheel :ezwheel:
  ;; * Home :home:

  ;; ** Capture and inbox
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

  ;; ** Display and key bindings
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

  ;; ** Refile
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

  ;; ** TODOs
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")))
  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  ;; ** Agenda
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

  ;; ** Beamer on Org Mode
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

  ;; ** Org Bibtex
  (setq org-ref-default-bibliography '("~/Zotero/my-library.bib")
        org-ref-pdf-directory "~/Zotero/storage"
        org-ref-bibliography-notes "~/Org/bibtex/notes.org")

  ;; -------
  ;; Org open PDF with Zathura
  ;; pacman -S zathura zathura-pdf-poppler zathura-ps zathura-cb zathura-djvu
  (setq org-ref-open-pdf-function
        (lambda (fpath)
          (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))

  ;; Set gT and gt to navigate between centaur-tabs
  ;; (setq global-flycheck-mode nil)
  (with-eval-after-load 'evil-maps
    (when (featurep 'tab-bar)
      (define-key evil-normal-state-map "gt" 'centaur-tabs-forward)
      (define-key evil-normal-state-map "gT" 'centaur-tabs-backward)))


  ;; Bitbake/Yocto
  (require 'bitbake)
  (setq auto-mode-alist (cons '("\\.bb$" . bitbake-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.inc$" . bitbake-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.bbappend$" . bitbake-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.bbclass$" . bitbake-mode) auto-mode-alist))

  ;; ROS
  (setq auto-mode-alist (cons '("\\.launch$" . xml-mode) auto-mode-alist))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(evil-want-Y-yank-to-eol nil)
   '(package-selected-packages
     '(bitbake langtool zeal-at-point x86-lookup web-mode vimrc-mode unicode-fonts ucs-utils font-utils typit mmt tagedit systemd sudoku stickyfunc-enhance srefactor sql-indent spray slim-mode slack circe oauth2 scss-mode sass-mode pug-mode powershell persistent-soft pcache pacmacs orgit org-rich-yank org-projectile org-category-capture org-present org-pomodoro org-mime org-download org-cliplink org-brain opencl-mode nov esxml nasm-mode impatient-mode simple-httpd helpful elisp-refs helm-org-rifle helm-dash dash-docs helm-css-scss haml-mode gnuplot glsl-mode flyspell-correct-helm flyspell-correct evil-org erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emojify emoji-cheat-sheet-plus emmet-mode elfeed-org elfeed-goodies ace-jump-mode noflet elfeed dtrt-indent deft dap-mode bui dactyl-mode cuda-mode copy-as-format confluence xml-rpc company-web web-completion-data company-glsl company-emoji auto-dictionary adoc-mode markup-faces 2048-game yatemplate yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode wolfram-mode winum which-key web-beautify vterm volatile-highlights vi-tilde-fringe vala-snippets vala-mode vagrant-tramp vagrant uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs-all-the-icons toml-mode toc-org thrift terminal-here symon symbol-overlay string-inflection stan-mode sphinx-doc spaceline-all-the-icons smeargle slime-company shell-pop scad-mode ron-mode restart-emacs realgud ranger rainbow-mode rainbow-identifiers rainbow-delimiters racer qml-mode pytest pyenv-mode py-isort prettier-js popwin poetry plantuml-mode pkgbuild-mode pippel pipenv pip-requirements pcre2el password-generator paradox pandoc-mode ox-pandoc overseer org-superstar org-ref open-junk-file nameless multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode matlab-mode markdown-toc magit-svn magit-section magit-gitflow lsp-ui lsp-treemacs lsp-python-ms lsp-pyright lsp-origami lsp-latex lsp-julia lorem-ipsum logcat live-py-mode link-hint julia-repl json-navigator insert-shebang indent-guide importmagic ibuffer-projectile hybrid-mode hungry-delete hoon-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org helm-mu helm-mode-manager helm-make helm-lsp helm-ls-git helm-gitignore helm-git-grep helm-flx helm-descbinds helm-ctest helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate google-c-style golden-ratio gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ gh-md fuzzy forge font-lock+ flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flycheck-bashate flx-ido fish-mode fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view eshell-z eshell-prompt-extras esh-help emr elisp-slime-nav ein editorconfig ebuild-mode dumb-jump dotenv-mode dockerfile-mode docker disaster dired-quick-sort diminish devdocs define-word cython-mode csv-mode cpp-auto-include company-ycmd company-shell company-rtags company-reftex company-math company-lua company-c-headers company-auctex company-anaconda common-lisp-snippets column-enforce-mode color-identifiers-mode cmake-mode cmake-ide clean-aindent-mode centered-cursor-mode centaur-tabs ccls cargo browse-at-remote blacken auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk arduino-mode aggressive-indent ace-link ace-jump-helm-line ac-ispell)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
