let moria_style = 'white'
colorscheme moria
syntax on             " Enable syntax highlighting
filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins 
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set nocompatible
helptags ~/.vim/doc
set list listchars=tab:\|_,trail:~
"highlight SpecialKey ctermfg=DarkGray
"set laststatus=2
"set statusline=%#StatusLine#%{GitBranch()}
