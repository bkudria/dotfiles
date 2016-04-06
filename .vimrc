set nocompatible
set cursorline
set laststatus=2
set shortmess+=I
set hidden

set guioptions-=m
set guioptions-=r
set guioptions-=T

set guicursor+=a:blinkon0

set list
set listchars=tab:\»\ ,trail:·

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
set cmdheight=1
set scrolloff=20
set shortmess=aOsTWI

set updatetime=500

syntax enable

"" Persistent undo
set undofile
set undodir=~/.vim/undodir

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
source ~/.vim/functions.vim
source ~/.vim/keys.vim
source ~/.vim/operators.vim

cd ~

" backup to ~/.tmp
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

set background=dark
colorscheme solarized

hi! link SignColumn   Background
hi! link NonText      Background
hi! link SpecialKey   Conceal
hi! link LineNr       Conceal
hi! link CursorLineNr Background

hi! MatchParen guibg='#073642' guifg='#b58900'

hi! DiffAdd    guibg='#002b36'
hi! DiffChange guibg='#002b36'
hi! DiffDelete guibg='#002b36'
