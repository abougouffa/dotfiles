autoload -Uz compinit
compinit

if command -v jj &> /dev/null; then
    source <(jj util completion zsh)
fi

# -*- mode: sh -*-

# When logging via Tramp it will look for patterns to detect if a shell is
# present. Fancy shell prompts aren't taken into account.
if [[ $TERM == "dumb" ]]; then
    unsetopt zle
    PS1='$ '
    return
fi

export ZSH="$HOME/.oh-my-zsh"

# Source extra commands
source "$HOME/.shell_extras"

# Typewritten customizations
TYPEWRITTEN_RELATIVE_PATH="adaptive"
TYPEWRITTEN_CURSOR="underscore"

ZSH_THEME="typewritten/typewritten"

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
ZSH_CUSTOM=$HOME/.config/my_ohmyzsh_customizations

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  zsh-autosuggestions
  zsh-navigation-tools
  zsh-interactive-cd
  archlinux
  ssh-agent
  sudo
  docker
  systemd
  tmux
  python
  pip
  rust
  repo
  cp
  rsync
  z
)

source "$ZSH/oh-my-zsh.sh"

# Aliases
alias zshconfig="vim $HOME/.zshrc"
alias ohmyzsh="ranger $ZSH"

command -v direnv &>/dev/null && eval "$(direnv hook zsh)"

command -v fzf &> /dev/null && eval "$(eval fzf --zsh)"

# # Automatically added by the Guix install script.
# if [ -n "$GUIX_ENVIRONMENT" ]; then
#     if [[ $PS1 =~ (.*)"\\$" ]]; then
#         PS1="${BASH_REMATCH[1]} [env]\\\$ "
#     fi
# fi
