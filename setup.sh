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
ln -s ~/.dotfiles/emacs ~/.emacs
ln -s ~/.dotfiles/emacs.d ~/.emacs.d
ln -s ~/.dotfiles/zshrc ~/.zshrc
mkdir -p ~/.xmonad
ln -s ~/.dotfiles/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/.dotfiles/xmobar.hs ~/.xmonad/xmobar.hs
ln -s ~/.dotfiles/gitconfig ~/.gitconfig
