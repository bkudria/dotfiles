filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'vim-scripts/scratch.vim'
Bundle 'scrooloose/syntastic',
let g:syntastic_check_on_open=1

Bundle 'Lokaltog/vim-powerline',
Bundle 'tpope/vim-fugitive',
Bundle 'tpope/vim-rails',
Bundle 'tpope/vim-endwise',
Bundle 'tpope/vim-surround',
Bundle 'tpope/vim-repeat'
Bundle 'Raimondi/delimitMate',
Bundle 'godlygeek/tabular'
Bundle 'mileszs/ack.vim'
Bundle 'kana/vim-textobj-user',
Bundle 'nelstrom/vim-textobj-rubyblock',
runtime macros/matchit.vim

Bundle 'wincent/Command-T',
let g:CommandTScanDotDirectories=1
let g:CommandTMatchWindowReverse=1
let g:CommandTMaxFiles=25000

Bundle 'jeffkreeftmeijer/vim-numbertoggle'
Bundle 'grep.vim'
let Grep_Xargs_Options = '-0'

Bundle 'ervandew/supertab'
inoremap <expr> <Esc>  pumvisible() ? "\<C-e>" : "\<Esc>"
inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>   pumvisible() ? "\<C-p>" : "\<Up>"



"Color Scheme"
Bundle 'altercation/vim-colors-solarized'
syntax enable
set background=light
colorscheme solarized

filetype plugin indent on
