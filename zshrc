# avoid problems with emacs tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
ZSH_THEME="agnoster"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(nyan rvm web-search lol last-working-dir history-substring-search rails bundler ruby git cap command-not-found gem git-extras github screen vagrant vi-mode wd autojump fabric docker)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

DEFAULT_USER="svk"

autoload -U zmv
alias mmv='noglob zmv -W'

# enable rvm
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# tell Java that XMonad is non-reparenting (prevents blank windows of java applications)
export _JAVA_AWT_WM_NONREPARENTING=1


export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

PATH=$PATH:/usr/local/eclipse # Path to eclipse

PATH=$PATH:$HOME/.cabal/bin # Path to cabal
PATH=$PATH:$HOME/.cabal-sandbox/bin # Path to project specific cabal sandbox
PATH=$PATH:$HOME/.dotfiles/bin # Path to own scripts
alias kb=keyboard-layout

export EDITOR=vim

# fix for history-substring-search, see https://github.com/robbyrussell/oh-my-zsh/issues/1433#issuecomment-38266082
zmodload zsh/terminfo
bindkey "$terminfo[cuu1]" history-substring-search-up
bindkey "$terminfo[cud1]" history-substring-search-down
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward

alias dockercleancontainers="docker ps -a -f status=exited -q | xargs docker rm"
alias dockercleanimages="docker images -f dangling=true -q | xargs docker rmi"
alias dockerclean="dockercleancontainers && dockercleanimages"

# ps + grep.
# see https://github.com/blueyed/oh-my-zsh/blob/a08181210b47625efdc8480e628b0155bff392c9/lib/aliases.zsh#L10-L18
pg() {
  local pids
  pids=$(pgrep -f $@)
  if [[ ! -n $pids ]]; then
    echo "No processes found." >&2; return 1
  fi
  ps up $(pgrep -f $@)
}
