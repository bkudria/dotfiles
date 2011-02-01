set nocompatible
set backspace=indent,eol,start
set autoindent
set history=100
set ruler
set showcmd
set incsearch
set mouse=a
set relativenumber

syntax on
filetype plugin indent on

set shortmess+=I

colorscheme summerfruit
set list
set listchars=tab:▸—,eol:$,trail:·,extends:»,precedes:«
highlight NonText guifg=#dddddd guibg=#ffffff
highlight SpecialKey guifg=#dddddd guibg=#ffffff

autocmd bufwritepost .vimrc source $MYVIMRC

set guioptions-=M  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set laststatus=2
