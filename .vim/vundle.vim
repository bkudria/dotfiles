filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

"Color Scheme"
Bundle 'altercation/vim-colors-solarized'
syntax enable
set background=light
colorscheme solarized

Bundle 'git://git.wincent.com/command-t.git'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-surround'

filetype plugin indent on