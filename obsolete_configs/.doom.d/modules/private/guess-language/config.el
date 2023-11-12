;;; checkers/guess-language/config.el -*- lexical-binding: t; -*-

(use-package! guess-language
  :ensure t
  :hook (text-mode . #'guess-language-mode)
  :init
  (setq guess-language-languages '(ar en fr)
        guess-language-min-paragraph-length 50)
  :config
  (defun +set-buffer-language (lang beg end)
    (let ((buff-lang (pcase lang
                       ('ar "ar")
                       ('en "en")
                       ('fr "fr")
                       (_   "auto"))))
      (message "Detected language: %s, setting `lsp-ltex-language' to \"%s\""
               ;; Get language name from langcodes
               (catch 'l-name (dolist (l guess-language-langcodes)
                                (when (eq lang (car l))
                                  (throw 'l-name (nth 2 l)))))
               buff-lang)
      (setq-local lsp-ltex-language buff-lang
                  flymake-languagetool-language buff-lang)))

  (add-hook 'guess-language-after-detection-functions #'+set-buffer-language))
