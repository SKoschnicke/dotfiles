#!/bin/sh

ln -s ~/.dotfiles/vim ~/.vim
ln -s ~/.dotfiles/vimrc ~/.vimrc
ln -s ~/.dotfiles/gvimrc ~/.gvimrc
ln -s ~/.dotfiles/tmux.conf ~/.tmux.conf
# you need tmux >=1.9:
# sudo add-apt-repository -y ppa:pi-rho/dev
# sudo apt-get update
# sudo apt-get install -y tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -s ~/.dotfiles/screenrc ~/.screenrc
ln -s ~/.dotfiles/Xresources ~/.Xresources
ln -s ~/.dotfiles/xsessionrc ~/.xinitrc
#ln -s ~/.dotfiles/emacs ~/.emacs
#ln -s ~/.dotfiles/emacs.d ~/.emacs.d
ln -s ~/.dotfiles/zshrc ~/.zshrc
ln -s ~/.dotfiles/zshenv ~/.zshenv
mkdir -p ~/.xmonad/bin
ln -s ~/.dotfiles/bin/keyboard-layout ~/.xmonad/bin/keyboard-layout
ln -s ~/.dotfiles/bin/select-screenshot ~/.xmonad/bin/select-screenshot
ln -s ~/.dotfiles/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/.dotfiles/xmobar.hs.`hostname` ~/.xmonad/xmobar.hs
ln -s ~/.dotfiles/gitconfig ~/.gitconfig
ln -s ~/.dotfiles/pentadactylrc ~/.pentadactylrc
mkdir -p ~/.pentadactyl
ln -s ~/.dotfiles/pentadactyl/colors ~/.pentadactyl/colors
mkdir -p ~/.config/clipit
ln -s ~/.dotfiles/clipitrc ~/.config/clipit/clipitrc
ln -s ~/.dotfiles/yaourtrc ~/.yaourtrc
ln -s ~/.dotfiles/muttrc ~/.muttrc
ln -s ~/.dotfiles/spacemacs ~/.spacemacs
ln -s ~/.dotfiles/dunstrc ~/.dunstrc
ln -s ~/.dotfiles/offlineimaprc ~/.offlineimaprc
ln -s ~/.dotfiles/eclimrc ~/.eclimrc
ln -s ~/.dotfiles/gitignore ~/.gitignore-global
ln -s ~/.dotfiles/vimperatorrc ~/.vimperatorrc
ln -s ~/.dotfiles/yaourtrc ~/.yaourtrc
mkdir -p ~/.config/ranger
ln -s ~/.dotfiles/ranger.conf ~/.config/ranger/rc.conf
ln -s ~/.dotfiles/shell-startup.sh ~/.shell-startup.sh
mkdir -p ~/.config/trizen
ln -s ~/.dotfiles/trizen.conf ~/.config/trizen/trizen.conf
mkdir -p ~/.config/kitty
ln -s ~/.dotfiles/kitty.conf ~/.config/kitty/kitty.conf
mkdir -p ~/.config/teiler
ln -s ~/.dotfiles/teiler.conf ~/.config/teiler/config
mkdir -p ~/.config/vifm
ln -s ~/.dotfiles/vifm ~/.config/vifm/vifmrc
ln -s ~/.dotfiles/vifm-scripts/extract-files ~/.config/vifm/scripts/
ln -s ~/.dotfiles/vifm-scripts/mount-avfs ~/.config/vifm/scripts/
