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
Bundle 'kana/vim-textobj-user',
Bundle 'nelstrom/vim-textobj-rubyblock',
Bundle 'wincent/Command-T',
let g:CommandTScanDotDirectories=1

"Color Scheme"
Bundle 'altercation/vim-colors-solarized'
syntax enable
set background=light
colorscheme solarized

filetype plugin indent on
