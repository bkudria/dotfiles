filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'L9',
Bundle 'FuzzyFinder',
Bundle 'scrooloose/syntastic',
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
Bundle 'wincent/Command-T',
let g:CommandTScanDotDirectories=1
let g:CommandTMatchWindowReverse=1

Bundle 'jeffkreeftmeijer/vim-numbertoggle'
Bundle 'grep.vim'
Bundle 'ervandew/supertab'
let g:SuperTabDefaultCompletionType = "context"

"Color Scheme"
Bundle 'altercation/vim-colors-solarized'
syntax enable
set background=light
colorscheme solarized

filetype plugin indent on
