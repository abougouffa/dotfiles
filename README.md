# My Dotfiles

![Doom Emacs Logo, CC-BY](dot_doom.d/assets/doom-emacs-dark.svg)

Contains my configuration files for **Zsh**, **Emacs**, **Vim**, **Alacritty** 
and other Linux related stuff.

If you need to reuse some of these configurations, you will need to modify some 
directories and add some user specific information (usernames, passwords...)

The main file is [`.doom.d/config.org`](dot_doom.d/config.org), (available also
as a [PDF file](dot_doom.d/config.pdf), it contains the literal configuration 
for [Doom Emacs](https://github.com/hlissner/doom-emacs), and I use it to 
generate some other user configuration files (define aliases, environment 
variables, user tools, Git configuration...).


## How to install

Since
[55c92810](https://github.com/abougouffa/dotfiles/commit/55c928109e6198e48749acca9171cf70372ce806),
I'm using [**chezmoi**](https://chezmoi.io) to manage my Dotfiles.

Now the Dotfiles can be installed using:

``` shell
sudo pacman -S chezmoi
chezmoi init --apply abougouffa
```


## Emacs stuff

- You will need to install [Chemacs2](https://github.com/plexus/chemacs2) to
  `.emacs.d`:

```shell
[ -f ~/.emacs ] && mv ~/.emacs ~/.emacs.bak
[ -d ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.default

git clone https://github.com/plexus/chemacs2.git ~/.emacs.d
```

- Install [Doom Emacs](https://github.com/hlissner/doom-emacs) to
  `~/.config/emacs.doom` instead of `.emacs.d`:

```shell
git clone https://github.com/hlissner/doom-emacs.git ~/.config/emacs.doom

~/.config/emacs.doom/bin/doom install
```

- You can install other Emacs distributions like
  [Spacemacs](https://github.com/syl20bnr/spacemacs); lets clone it to
  `~/.config/emacs.spacemacs`, however, I'm not using it anymore, it can just be
  a fall back solution if I mess up my Doom config and I want to do something
  quickly:

```shell
git clone https://github.com/syl20bnr/spacemacs.git ~/.config/emacs.spacemacs
```

- You can now customize `~/.emacs-profiles.el` if you want to.

---

Happy hacking!
