let moria_style = 'white'
"colorscheme moria
set background=dark " because of strange xresources, this means actually light
colorscheme solarized
syntax on             " Enable syntax highlighting
filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins 
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set nocompatible
set nobackup          " make no backups when writing
set nowritebackup
set noswapfile        " Disable creation of swap files
set smartcase         " case-sensitive if search contains an uppercase character
helptags ~/.vim/doc
set list listchars=tab:\|_,trail:~
"highlight SpecialKey ctermfg=DarkGray
"set laststatus=2

" use css syntax highlighting for .less files
au BufRead,BufNewFile *.less setfiletype css
au BufRead,BufNewFile *.thtml setfiletype php
" files named buildfile belong to buildr and use ruby syntax
au BufRead,BufNewFile buildfile setfiletype ruby

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*
" refresh view, Ctrl-X Ctrl-R
cnoremap <C-X><C-L> <C-R>=GetVimCmdOutput('redraw')<CR>

let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"

set nobackup
set noswapfile

function IdeFindTextMate()
  let g:FuzzyFinderOptions.Base.key_open = '<CR>'
  let g:FuzzyFinderOptions.Base.key_open_split = '<C-j>'
  exe "FuzzyFinderTextMate"
endfunction

function IdeSplitFindTextMate()
  let g:FuzzyFinderOptions.Base.key_open = '<C-j>'
  let g:FuzzyFinderOptions.Base.key_open_split = '<CR>'
  exe "FuzzyFinderTextMate"
endfunction

map <silent> ,e :call IdeFindTextMate()<CR>
map <silent> ,s :call IdeSplitFindTextMate()<CR>

map <C-X>t :FuzzyFinderTextMate<CR>

" syntastic settings
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1


map <silent> ,t :TlistToggle<CR>

set statusline=%f       "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file"
set statusline+=\ %{fugitive#statusline()}

" let :w!! save file as sudoer
cmap w!! %!sudo tee > /dev/null %
