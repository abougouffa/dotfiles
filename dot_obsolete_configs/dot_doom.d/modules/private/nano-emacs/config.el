;;; ui/nano-emacs/config.el -*- lexical-binding: t; -*-

(use-package! nano-faces
  :init
  (defun +nano-emacs-setup ()
    (require 'nano-faces)
    (require 'nano-layout)
    (nano-faces)
    (require 'nano-theme-light)
    (nano-theme-set-light)
    (require 'nano-theme)
    (require 'nano-modeline)

    (when (display-graphic-p)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

  (+nano-emacs-setup)

  (add-hook! 'doom-init-ui-hook #'+nano-emacs-setup)
  (setq nano-font-family-monospaced "Iosevka Fixed Curly Slab"
        nano-font-family-proportional nil
        nano-font-size 16))
