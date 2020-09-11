#!/bin/bash

DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

echo "Dotfiles in '$DOTFILES_DIR'"

echo "Creating symlink to dot.doom.d"
ln -s "$DOTFILES_DIR/dot.doom.d" ~/.doom.d

