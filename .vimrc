set nocompatible
set showcmd
set showmatch
set exrc
set secure
set relativenumber
set cursorline
set shortmess+=I
set guioptions-=m

set encoding=utf-8
set guifont=Consolas:h13

syntax enable
filetype plugin indent on


"" Whitespace
set nowrap
set tabstop=2 shiftwidth=2
set expandtab
set backspace=indent,eol,start

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

source .vim/vundle.vim
