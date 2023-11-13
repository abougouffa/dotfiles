DOTFILES_DIR=.
EMACS=emacs
EMACS_BATCH=emacs --batch

tangle:
	$(EMACS_BATCH) --eval '(with-current-buffer (find-file-noselect "literate-config.org") (org-babel-tangle))'

all: tangle
