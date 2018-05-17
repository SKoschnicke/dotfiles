PATH=$PATH:/usr/local/eclipse # Path to eclipse
PATH=$PATH:$HOME/.cabal/bin # Path to cabal
PATH=$PATH:$HOME/.cabal-sandbox/bin # Path to project specific cabal sandbox
PATH=$PATH:$HOME/.dotfiles/bin # Path to own scripts
PATH=$PATH:$HOME/bin # Path to local binaries
PATH=$PATH:$HOME/.gem/ruby/2.3.0/bin
PATH=$PATH:$HOME/.gem/ruby/2.4.0/bin
PATH=$PATH:$HOME/.gem/ruby/2.5.0/bin
PATH=$PATH:$HOME/.local/bin
export PATH
export NODE_ENV="development"
export -U PATH # -U for deduplication
if [[ $HOST == "sven-uni" ]]; then
  export CUPS_SERVER=printhost.informatik.uni-kiel.de:631
fi
