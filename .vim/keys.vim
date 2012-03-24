let mapleader=","
let g:mapleader=","

map <up>    <nop>
map <down>  <nop>
map <left>  <nop>
map <right> <nop>

map <leader>f :CommandT<cr>
if has("gui_macvim")
  macmenu &File.Open\.\.\. key=<nop>
  map <D-o> :CommandT<CR>

  macmenu &File.Close key=<nop>
  map <D-w> :bdelete<cr>
endif

nnoremap ; :

map <F1> :FufHelp<cr>
map <F2> :
map <F3> :CommandTBuffer<cr>
map <F4> :ScratchOpen<cr>
nnoremap <leader><leader> <c-^>
map <F10> :Gstatus<cr>
map <F11>g :Rgrep<cr>

nnoremap \ :noh<cr>

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-w>1 :only<cr>

map ~ :cd ~<cr>
