set nocompatible
set cursorline
set laststatus=2
set shortmess+=I
set hidden

set guioptions-=m
set guioptions-=r
set guioptions-=T

set list
set listchars=tab:\|\ ,trail:·

set clipboard=unnamed

if exists("&relativenumber")
  set relativenumber
else
  set number
endif

set wildmode=longest,full
set wildchar=<Tab>

set encoding=utf-8
set guifont=Source_Code_Pro_Medium:h14

"" Whitespace
set nowrap

"" Searching
set hlsearch
set ignorecase
set smartcase
set gdefault

set iskeyword+=!
set cmdheight=2
set scrolloff=20
set shortmess=aOsTWI

augroup MyAutoCmd
  autocmd!
augroup END

autocmd BufWritePost * stopinsert   " Leave insert mode on save

set tags=.git/tags,./.git/tags
" Regenerate ctags on write, if we can
autocmd BufWritePost *
      \ if exists('b:git_dir') && executable(b:git_dir.'/hooks/ctags') |
      \   call system('"'.b:git_dir.'/hooks/ctags" &') |
      \ endif


source ~/.vim/packages.vim
source ~/.vim/keys.vim
source ~/.vim/functions.vim
source ~/.vim/operators.vim

cd ~

" backup to ~/.tmp
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup



syntax enable
set background=dark
colorscheme solarized
hi! link SignColumn Background
