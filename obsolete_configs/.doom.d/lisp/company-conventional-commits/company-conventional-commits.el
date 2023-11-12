;;; company-conventional-commits.el --- Company backend for git commits -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27"))
;; Keywords: vc
;; URL: https://github.com/abougouffa/company-conventional-commits

;;; Commentary:

;; Provide completion backend for commit messages, based on a flavored
;; ConventionalCommits (https://www.conventionalcommits.org/en/v1.0.0).

;;; Code:

(require 'cl-lib)

(defvar company-conventional-commits-keywords
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

(defun company-conventional-commits--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-conventional-commits--candidates (prefix)
  (let (res)
    (dolist (item company-conventional-commits-keywords)
      (when (string-prefix-p prefix (car item))
        (push (company-conventional-commits--make-candidate item) res)))
    res))

(defun company-conventional-commits--meta (candidate)
  (format "This will use %s of %s"
          (get-text-property 0 'meta candidate)
          (substring-no-properties candidate)))

(defun company-conventional-commits--annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

;;;###autoload
(defun company-conventional-commits (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-conventional-commits))
    (prefix (company-grab-symbol-cons "\\.\\|->" 2))
    (candidates (company-conventional-commits--candidates arg))
    (annotation (company-conventional-commits--annotation arg))
    (meta (company-conventional-commits--meta arg))))

(provide 'company-conventional-commits)

;;; company-conventional-commits.el ends here
