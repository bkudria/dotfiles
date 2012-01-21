set nocompatible
set nohidden
set autoread
set showcmd
set showmatch
set exrc
set secure
set cursorline
set laststatus=2
set shortmess+=I
set guioptions-=m
if exists("&relativenumber")
  set relativenumber
else
  set number
endif

set wildmenu
set wildmode=longest,full
set wildchar=<Tab>

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

source ~/.vim/vundle.vim
source ~/.vim/keys.vim

cd ~
autocmd! bufwritepost .vimrc source ~/.vimrc
