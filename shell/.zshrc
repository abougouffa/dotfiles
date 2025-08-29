# -*- mode: sh -*-

# From: https://stackoverflow.com/a/54951844
# When logging via Tramp it will look for patterns to detect if a shell is
# present. Fancy shell prompts aren't taken into account.
# When logging via Tramp it will look for patterns to detect if a shell is
# present. Fancy shell prompts aren't taken into account.
# Now dumb terminals
if [[ "${TERM}" == "dumb" ]]; then
    # Here put anything you want to run in any dumb terminal,
    # even outside Emacs.
    alias lsF='ls -F'

    # echo "DUMB"
    # Now, just configs for shells inside Emacs
    case ${INSIDE_EMACS/*,/} in
    comint)
        # echo "COMINT"
        ;;
    tramp)
        # echo "TRAMP"
        unsetopt zle
        PS1='$ '
        ;;
    term*)
        # echo "ANSI-TERM (maybe!)"
        # For M-x ansi-term, etc., you get a value like
        #   25.2.2,term:0.96, but those shouldn't coincide with
        #   TERM being `dumb`, so warn....
        echo "We somehow have a dumb Emacs terminal ${INSIDE_EMACS/*,/}" >&2
        ;;
    "")
        # Empty means we're $TERM==dumb but not in Emacs, do nothing
        ;;
    *)
        # We shouldn't get here, so write a warning so we can
        # figure out how else Emacs might be running a shell,
        # but send it to stderr so that it won't break anything
        echo "Something is wrong: INSIDE_EMACS is ${INSIDE_EMACS}" >&2
        ;;
    esac

    # finish shell setup for dumb now--the rest of the file will
    # be skipped
    return
fi

# Enable the subsequent settings only in interactive sessions
case $- in
*i*)
    # Interactive mode, do nothing
    ;;
*)
    # Non interactive mode, return immediately
    return
    ;;
esac

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

command -v fzf &> /dev/null && eval "$(eval fzf --zsh)"

# # Automatically added by the Guix install script.
# if [ -n "$GUIX_ENVIRONMENT" ]; then
#     if [[ $PS1 =~ (.*)"\\$" ]]; then
#         PS1="${BASH_REMATCH[1]} [env]\\\$ "
#     fi
# fi

autoload -Uz compinit
compinit

if command -v jj &> /dev/null; then
    source <(jj util completion zsh)
fi

command -v direnv &>/dev/null && eval "$(direnv hook zsh)"
