# avoid problems with emacs tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# NOTE that fasd needs the executable fasd installed
# NOTE do NOT enable the tmux plugin! It breaks the last-working-dir functionality
#plugins=(last-working-dir zsh-navigation-tools rvm web-search bundler ruby git gem git-extras github vi-mode wd fabric docker docker-compose archlinux colorize alias-tips fasd zsh-autosuggestions dircycle safe-paste ssh-agent z extract mise)
#ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# to install autosuggestions plugin:
# git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# to install alias-tips plugin:
# git clone https://github.com/djui/alias-tips ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/alias-tips

source /opt/homebrew/opt/zinit/zinit.zsh

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

# Autosuggestions
zinit ice wait lucid atload'_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions

# Plugin history-search-multi-word loaded with investigating.
#zinit load zdharma-continuum/history-search-multi-word

# allow cd ... = cd ../.. etc
zinit autoload'#manydots-magic' for twang817/zsh-manydots-magic 

# Two regular plugins loaded without investigating.

# Syntax Highlight
zinit light zdharma-continuum/fast-syntax-highlighting

# Alias Tips
zinit light djui/alias-tips
export ZSH_PLUGINS_ALIAS_TIPS_TEXT="💡 Alias tip: "

# Git
zi snippet OMZL::git.zsh
zi snippet OMZP::git

# Git Fuzzy
zinit ice as"program" pick"bin/git-fuzzy"
zinit light bigH/git-fuzzy

# Mise
zi snippet OMZP::mise

# Z (like autojump)
zi snippet OMZP::z

# lolcats aliases
zi snippet OMZP::lol

# remember last working directory in newly started shells
zi snippet OMZP::last-working-dir

# emacs shortcuts
#zi snippet OMZP::emacs
# vi mode
zi snippet OMZP::vi-mode

# fzf
zi snippet OMZP::fzf

# fzf ssh
zinit light sunlei/zsh-ssh

# Load starship theme
# line 1: `starship` binary as command, from github release
# line 2: starship setup at clone(create init.zsh, completion)
# line 3: pull behavior same as clone, source init.zsh
zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh"
zinit light starship/starship

zinit load atuinsh/atuin
eval "$(atuin init zsh)"

setopt  autocd autopushd
# Customize to your needs...

DEFAULT_USER="svk"

# autoload -U zmv
# alias mmv='noglob zmv -W'

# # Settings for the zsh-navigation-tools
# autoload znt-history-widget
# zle -N znt-history-widget
# bindkey "^R" znt-history-widget
# #znt_cd_hotlist=( "~/development/pa/rho" "~/development/pa/psi" "~/development/sc/webapp"
#                  #"~/development/sc/graphical_client" "~/development/sc/client_server"
#                #)
# #zle -N znt-cd-widget
# #bindkey "^J" znt-cd-widget

# call file manager with Ctrl-K
# jumps to the selected directory when quit
start_filemanager() {
  local dst=$(command vifm --choose-dir - $PWD < $TTY)
  if [ -z "$dst" ]; then
    echo 'Directory picking cancelled/failed'
    return 1
  fi
  cd "$dst"
  zle reset-prompt
}
zle -N start_filemanager
bindkey "^K" start_filemanager

# always do pushd when cding, so you can always navigate back by calling popd (even multiple times, which doesn't work with cd -)
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
# enable rvm
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"


# dircycle with Ctrl-H and Ctrl-L
#bindkey -c '^H' cd ..
#bindkey '^L' insert-cycledright

# tell Java that XMonad is non-reparenting (prevents blank windows of java applications)
export _JAVA_AWT_WM_NONREPARENTING=1

function gitrmtag () {
  declare -a refs
  local index=1
  for tag in $@
  do
    refs[index++]=":refs/tags/$tag"
  done
  git push origin "${refs[@]}" && git tag -d "$@"
}

export EDITOR="nvim"

#alias docker=podman
alias dockercleancontainers="docker ps -a -f status=exited -q | xargs docker rm"
alias dockercleanimages="docker images -f dangling=true -q | xargs docker rmi"
alias dockerclean="dockercleancontainers && dockercleanimages"
alias dc="docker-compose"

alias pa="cd $HOME/development/pa"
alias sc="cd $HOME/development/sc"
alias ls="TERM=xterm-256color exa"
alias ll="TERM=xterm-256color exa --git -l"
alias db="dropbox-cli"

alias cat=bat
#alias find=fd
alias gcb="./gradlew clean build"
alias gct="./gradlew clean check"
alias gcr="./gradlew clean run"

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

if [[ "$(hostname)" == "daltigoth" ]]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

# initialize rbenv (ruby version manager)
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
alias cherry="xmodmap ~/.Xmodmap_cherry"
export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"
unalias gr # zsh git plugin defines this alias but we want to use the gr tool

export GOPATH=$HOME/go
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:/home/linuxbrew/.linuxbrew/bin"
export PATH="$PATH:$HOME/.emacs.d/bin"
if [[ "$(hostname)" == "Svens-Air.localdomain" || "$(hostname)" == "istar.localdomain" || "$(hostname)" == "Sven’s-MacBook-Air" ]]; then
  export PATH="$PATH:$(brew --prefix python)/libexec/bin"
  export PATH="$PATH:/Users/sven/Library/Python/3.11/bin"
fi

export BAT_THEME="Monokai Extended Light"

export TLDR_COLOR_BLANK="white"
export TLDR_COLOR_NAME="cyan"
export TLDR_COLOR_DESCRIPTION="yellow"
export TLDR_COLOR_EXAMPLE="green"
export TLDR_COLOR_COMMAND="blue"
export TLDR_COLOR_PARAMETER="magenta"
export TLDR_CACHE_ENABLED=1
export TLDR_CACHE_MAX_AGE=720

export MOZ_ENABLE_WAYLAND=1


export PATH="$PATH:$HOME/.npm-global/bin"
export PATH="$PATH:$HOME/.config/composer/vendor/bin"

alias wlan="wicd-cli --wireless"

alias f=frontastic
alias mp=multipass

export AICHAT_CONFIG_DIR=$HOME/.config/aichat
alias ai="aichat"
alias aic="aichat --role coder"
alias ais="aichat --role shell"

source $HOME/.config/broot/launcher/bash/br
#bindkey -s "^J" "br^M"
#
alias vi=nvim

# refresh timeout every time using sudo
alias sudo='sudo  -v; sudo '

sshfm() {
  if (( # == 0 )); then
    kitty +kitten ssh vagrant@$(grep remoteserverhostname frontastic.toml|cut -d\' -f2)
  else
    kitty +kitten ssh vagrant@$1
  fi
}

svrestart() {
  ssh vagrant@$(grep remoteserverhostname frontastic.toml|cut -d\' -f2) sudo supervisorctl restart all
}
# not working on mac, needs phpenv
#export PATH="$HOME/.phpenv/bin:$PATH"
#eval "$(phpenv init -)"

alias sshk='kitty +kitten ssh'

eval "$(direnv hook zsh)"
#eval "$(starship init zsh)"

alias cdr=grt
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"

alias gs="git branch | grep -v \"^\*\" | fzf --height=20% --reverse --info=inline | xargs git switch"
