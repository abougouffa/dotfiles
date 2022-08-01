;;; company-gitcommit.el --- Company backend for git commits -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27"))
;; Keywords: vc
;; URL: https://github.com/abougouffa/company-gitcommit

;;; Commentary:

;; Provide completion backend for commit messages, based on a flavored
;; ConventionalCommits (https://www.conventionalcommits.org/en/v1.0.0).

;;; Code:

(require 'cl-lib)

(defvar company-gitcommit-keywords
  '(("feat"     "New feature or extending an existing one")
    ("fix"      "A bug fix")
    ("docs"     "Documentation only changes")
    ("style"    "Changes that do not affect the meaning of the code")
    ("bump"     "Update package or external dependencies")
    ("nit"      "cosmetic changes to comments, formatting, with no effect on behavior")
    ("refactor" "A code change that neither fixes a bug nor adds a feature")
    ("pref"     "A code change that improves performance")
    ("test"     "Adding missing tests or correcting existing tests")
    ("build"    "Changes that affect the build system or external dependencies")
    ("ci"       "Changes to our CI configuration files and scripts")
    ("chore"    "Other changes that do not modify src or test files")
    ("tweak"    "Changes that affect defaults and user-facing behavior, but not drastically")
    ("release"  "TODO")
    ("merge"    "TODO")
    ("revert"   "Reverts a previous commit")))

(defun company-gitcommit--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-gitcommit--candidates (prefix)
  (let (res)
    (dolist (item company-gitcommit-keywords)
      (when (string-prefix-p prefix (car item))
        (push (company-gitcommit--make-candidate item) res)))
    res))

(defun company-gitcommit--meta (candidate)
  (format "This will use %s of %s"
          (get-text-property 0 'meta candidate)
          (substring-no-properties candidate)))

(defun company-gitcommit--annotation (candidate)
  (format " (%s)" (get-text-property 0 'meta candidate)))

;;;###autoload
(defun company-gitcommit (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-gitcommit))
    (prefix (company-grab-symbol-cons "\\.\\|->" 2))
    (candidates (company-gitcommit--candidates arg))
    (annotation (company-gitcommit--annotation arg))
    (meta (company-gitcommit--meta arg))))

(provide 'company-gitcommit)

;;; company-gitcommit.el ends here
