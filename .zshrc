# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Add ~/.emacs.d/bin to path (for DOOM Emacs stuff)
# export PATH=$PATH:$HOME/.emacs.d/bin
export PATH=$PATH:$HOME/.doom-emacs/bin

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="typewritten/typewritten" #"robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=3

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=$HOME/.my_ohmyzsh_customizations

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(archlinux rustup rust zsh-autosuggestions git docker sudo ripgrep)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
#

# Rename the "update_ohmyszh" function to "_uptate_ohmyzsh"
# eval "`declare -f update_ohmyzsh | sed '1s/.*/_&/'`"

# update_ohmyzsh () {
# 	# Stash customized stuff (mainly, my modified "amuse" theme)
# 	cd $ZSH && git stash
# 	_update_ohmyzsh
# 	# Pop the stash
# 	cd $ZSH && git stash pop
# }

# ALIASES ===========================================================
alias vim="nvim"
alias vi="nvim"
alias ec="emacsclient -t"
alias dotfiles='git --git-dir=$HOME/Projects/dotfiles/.git --work-tree=$HOME'
alias spacemacs='emacs --with-profile spacemacs'
alias prelude-emacs='emacs --with-profile prelude'

# Kitty settingprelude
# autoload -Uz compinit
# compinit
# Completion for kitty
# kitty + complete setup zsh | source /dev/stdin

# EXPORTS ===========================================================

# Enable Meta key + colors in minicom
export MINICOM='-m -c on'

export RUST_SRC_PATH=$HOME/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/
export PATH=$PATH:$HOME/.cargo/bin

# Add my manually installed libraries to CMake
# export CMAKE_PREFIX_PATH=$CMAKE_PREFIX_PATH:$HOME/libs/bin
export CMAKE_PREFIX_PATH=$HOME/sources-and-libs/build_installs
export PATH=$PATH:$HOME/.cargo/bin:$HOME/sources-and-libs/build_installs/bin

# NPM, Set installation path to local
NPM_PACKAGES="${HOME}/.npm-packages"

export PATH="$PATH:$NPM_PACKAGES/bin"

# Preserve MANPATH if you already defined it somewhere in your config.
# Otherwise, fall back to `manpath` so we can inherit from `/etc/manpath`.
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

# STARTUP STUFF =====================================================
# Install from AUR shell-color-scripts
# Display random graphics
colorscript -e 16

# Run fortune to display random quote
# fortune

# GPG Agent
# GPG_TTY=$(tty)
# export GPG_TTY
