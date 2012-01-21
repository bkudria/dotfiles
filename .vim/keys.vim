let mapleader=","
let g:mapleader=","

map <leader>f :FufFile<cr>
map <leader>t :CommandT<cr>
if has("gui_macvim")
  macmenu &File.Open key=<nop>
  map <D-o> :FufFile<CR>
endif

map <F3> :FufBuffer<cr>
nnoremap <leader><leader> <c-^>
