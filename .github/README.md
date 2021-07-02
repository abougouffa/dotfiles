# My Dotfiles

![Logo](.doom.d/assets/doom-emacs-dark.svg)

Contains my config files for Zsh, Emacs, Vim, and other Linux related configs.

If you need to use some of these configs, you will need to modify some dirs and add some user specific informations (usernames, passwords...)

## Emacs stuff
- Install Chemacs2 to `.emacs.d`:
```shell
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
[ -d ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.default
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d
```

- Install DOOM Emacs to `.doom-emacs` instead of `.emacs.d`:
```shell
git clone https://github.com/hlissner/doom-emacs.git .doom-emacs

./doom-emacs/bin/doom install
```

- Install Spacemacs to `.emacs-spacemacs`:
```shell
git clone https://github.com/syl20bnr/spacemacs.git .emacs-spacemacs
```

- You can now customize `~/.emacs-profiles.el` if you want to.

---
Happy hacking
