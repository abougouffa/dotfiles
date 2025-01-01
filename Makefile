DOTFILES_DIR=.
EMACS=emacs
EMACS_BATCH=emacs --batch

# Otherwise, using the generic "%" rule with directory names will show "make: 'dosbox' is up to date."
# See: https://stackoverflow.com/a/70550568
MAKEFLAGS += --always-make

tangle:
	$(EMACS_BATCH) --eval '(with-current-buffer (find-file-noselect "literate-config.org") (org-babel-tangle))'

%:
	stow -t $(HOME) $@

all: tangle
