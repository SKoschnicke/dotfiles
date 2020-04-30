"let moria_style = 'white'
"colorscheme moria
set background=light
colorscheme solarized
"colorscheme github
let g:molokai_original=1
"colorscheme proton
"colorscheme molokai
set guifont=Inconsolata\ Medium\ 9

" no right scrollbars
set guioptions=-R
set guioptions=-r
" no buttom scrollbar
set guioptions=-b
" no left scrollbar
set guioptions=-L
set guioptions=-l
" no toolbar
set guioptions=-T
" menu bar is hidden
set guioptions=-m
" put selected text in global clipboard
set guioptions=+a


"set t_Co=256          " explicitly tell vim that its a terminal supporting 256 colors
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
" set 78 character long lines with soft-wrap for text files
autocmd FileType text setlocal wrap
autocmd FileType text setlocal textwidth=78

set tags=tags;/

set encoding=utf-8

" use css syntax highlighting for .less files
au BufRead,BufNewFile *.less setfiletype css
au BufRead,BufNewFile *.thtml setfiletype php
" files named buildfile belong to buildr and use ruby syntax
au BufRead,BufNewFile buildfile setfiletype ruby
au BufRead,BufNewFile Vagrantfile setfiletype ruby

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*
" refresh view, Ctrl-X Ctrl-R
cnoremap <C-X><C-L> <C-R>=GetVimCmdOutput('redraw')<CR>

set nobackup
set noswapfile

" syntastic settings
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1
let g:syntastic_disabled_filetypes = ['haskell']


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

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*.hi,*.o   " for Linux/MacOSX
set wildignore+=.git\*,.hg\*,.svn\*         " for Windows"

" open nerdtree on startup
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Begin neocomplcache config =====
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" " Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" " Use underbar completion.
let g:neocomplcache_enable_underbar_completion = 1
" " Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Plugin key-mappings.
imap <C-k>     <Plug>(neocomplcache_snippets_expand)
smap <C-k>     <Plug>(neocomplcache_snippets_expand)
inoremap <expr><C-g>  neocomplcache#undo_completion()
inoremap <expr><C-l>  neocomplcache#complete_common_string()

" SuperTab like snippets behavior.
"imap <expr><TAB> neocomplcache#sources#snippets_complete#expandable() ?  "\<Plug>(neocomplcache_snippets_expand)" : pumvisible() ? "\<C-n>" : "\<TAB>"

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()

" AutoComplPop like behavior.
"let g:neocomplcache_enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplcache_enable_auto_select = 1
"let g:neocomplcache_disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<TAB>"
"inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_extensions = ['tag']

let g:vimroom_background = 'white'

" set correct wrapping for writing prose
"set wrap linebreak nolist

" better split navigation (CTRL-J instead of CTRL-W J)
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

autocmd FileType html,xml,erb source ~/.vim/plugin/closetag.vim

au BufRead,BufNewFile *.hamlet  setf hamlet
au BufRead,BufNewFile *.cassius setf cassius
au BufRead,BufNewFile *.lucius  setf lucius
au BufRead,BufNewFile *.julius  setf julius

inoremap jj <Esc>
