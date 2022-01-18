# My Dotfiles

![Doom Emacs Logo, CC-BY](doom-emacs-dark.svg)

Contains my configuration files for Zsh, Emacs, Vim, and other Linux related stuff.

If you need to reuse some of these configs, you will need to modify some directories and add some user specific information (usernames, passwords...)

The main file is [`.doom.d/config.org`](../.doom.d/config.org), it contains the
literal configuration for [Doom Emacs](https://github.com/hlissner/doom-emacs),
and I use it to generate some other user configuration files (define aliases, 
environment variables, user tools, Git config...).

## Emacs stuff

- You will need to install [Chemacs2](https://github.com/plexus/chemacs2) to
  `.emacs.d`: 

```shell
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
[ -d ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.default
git clone https://github.com/plexus/chemacs2.git ~/.emacs.d
```

- Install [Doom Emacs](https://github.com/hlissner/doom-emacs) to `.emacs-doom`
  instead of `.emacs.d`:

```shell
git clone https://github.com/hlissner/doom-emacs.git .doom-emacs

./doom-emacs/bin/doom install
```

- I do install [Spacemacs](https://github.com/syl20bnr/spacemacs) to
  `.emacs-spacemacs`, however, I'm not using it anymore, it can just be a fall
  back solution if I mess up my Doom config and I want to do something quickly:

```shell
git clone https://github.com/syl20bnr/spacemacs.git .emacs-spacemacs
```

- You can now customize `~/.emacs-profiles.el` if you want to.

---

Happy hacking!
