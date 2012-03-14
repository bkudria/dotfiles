set nocompatible
set autoread
set showcmd
set showmatch
set exrc
set secure
set cursorline
set laststatus=2
set shortmess+=I

set guioptions-=m
set guioptions-=r
set guioptions-=T

set list
set listchars=tab:\|\ ,trail:·

if exists("&relativenumber")
  set relativenumber
else
  set number
endif

set completeopt=menu,preview,longest

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

autocmd BufWritePost * stopinsert " Leave insert mode on save

source ~/.vim/vundle.vim
source ~/.vim/keys.vim
source ~/.vim/commands.vim

cd ~
