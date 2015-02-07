let mapleader=","
let g:mapleader=","

map <up>    <nop>
map <down>  <nop>
map <left>  <nop>
map <right> <nop>

nmap <D-left> ^
imap <D-left> <esc>I
nmap <D-right> $
imap <D-right> <esc>A

if has("gui_macvim") && has("gui_running")
  macmenu &File.Open\.\.\. key=<nop>
  map <D-o> :CtrlPCurWD<CR>

  macmenu &File.Save key=<nop>
  nmap <D-s> :write<CR>
  imap <expr> <D-s> pumvisible() ? neocomplete#close_popup() . "<ESC><D-s>" : "<ESC><D-s>"

  macmenu &File.Close key=<nop>
  nmap <D-w> :CommandW<cr>

  macmenu &Edit.Find.Find\.\.\. key=<nop>
  map <D-f> /
endif

inoremap <Space> <C-g>u<Space>

inoremap <expr> <ESC>  pumvisible() ? neocomplete#cancel_popup() : "\<ESC>"
imap <expr> <D-CR> pumvisible() ? "\<Tab>" . neocomplete#close_popup() : "\<C-g>u<CR>"
imap <expr> <D-1>  pumvisible() ? repeat("\<Tab>", 1) . neocomplete#close_popup() : ""
imap <expr> <D-2>  pumvisible() ? repeat("\<Tab>", 2) . neocomplete#close_popup() : ""
imap <expr> <D-3>  pumvisible() ? repeat("\<Tab>", 3) . neocomplete#close_popup() : ""
imap <expr> <D-4>  pumvisible() ? repeat("\<Tab>", 4) . neocomplete#close_popup() : ""
imap <expr> <D-5>  pumvisible() ? repeat("\<Tab>", 5) . neocomplete#close_popup() : ""
imap <expr> <D-6>  pumvisible() ? repeat("\<Tab>", 6) . neocomplete#close_popup() : ""

inoremap <expr> <up>   pumvisible() ? neocomplete#cancel_popup() . "\<up>"   : "\<up>"
inoremap <expr> <down> pumvisible() ? neocomplete#cancel_popup() . "\<down>" : "\<down>"

" Bubble single lines
nmap <C-Up> [e
nmap <C-Down> ]e
" Bubble in insert mode
imap <C-Up>   <C-o><C-Up>
imap <C-Down> <C-o><C-Down>
" Bubble multiple lines
vmap <C-Up> [egv
vmap <C-Down> ]egv

" re-indent single lines
nmap <C-Left> mp<al`p
nmap <C-Right> mp>al`p
" re-indent in insert mode
imap <C-Left> <C-o>mp<C-o><al<C-o>`p
imap <C-Right> <C-o>mp<C-o>>al<C-o>`p
" Reindent multiple lines
vmap <C-Left> <
vmap <C-Right> >

nmap << <ai
nmap >> >ai

" Ctrl-j/k deletes blank line below/above, and Alt-j/k inserts.
nnoremap <silent><D-Up>     m`:silent -g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent><D-S-Up>   m`:silent +g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent><D-Down>   :set paste<CR>m`O<Esc>``:set nopaste<CR>
nnoremap <silent><D-S-Down> :set paste<CR>m`o<Esc>``:set nopaste<CR>

map <F1> <leader>h
map <F2> :CtrlPBuffer<cr>
map <F3> :CtrlPMRUFiles<cr>
map <F4> :CtrlPBranch<cr>
map <F5> :e!<cr>
map <F6> :SemanticHighlightToggle<cr>

nmap gt g<c-]>
map <M-LeftMouse> <LeftMouse>gt

map - <Plug>(operator-replace)

map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

nmap K i<cr><esc>
nmap L i<cr><esc>[e

nnoremap P :put!<cr>==

map <leader>a   :Tabularize /
map <leader>a:  :Tabularize /:<cr>
map <leader>a,  :Tabularize /,\zs<cr>
map <leader>a=  :Tabularize /=<cr>
map <leader>a=> :Tabularize /=><cr>

map <leader>bd  :e .<cr>

map <leader>cc :ccl<cr>
map <leader>cd :CtrlPDir ~<cr>

let g:blockle_mapping = '<Leader>bt'

map <leader>d   <Plug>(operator-dash)
map <leader>dru <Plug>(operator-dash-ruby)
map <leader>dra <Plug>(operator-dash-rails)

map <leader>e <Plug>(operator-exec)

map <leader>gs :Gstatus<cr>
map <leader>gr :Gread<cr>
map <leader>gw :Gwrite<cr>
map <leader>gl :Gitv<cr>
map <leader>gcc :Gcommit<cr>
map <leader>gcm :Gcommit -m ""<Left>
map <leader>gpl :Git pull<cr>
map <leader>gps :Git push<cr>
map <leader>gom :CtrlPModified<CR>
map <leader>gob :CtrlPBranch<CR>
map <leader>gbl :Gblame<CR>
map <leader>gts :SignifyToggleGit<cr>
nmap <leader>gj <plug>(signify-next-hunk)
nmap <leader>gk <plug>(signify-prev-hunk)

map <leader>f :Rgrep <c-r>* *.<c-r>=expand('%:e')<cr><cr>
map <leader>ff :Rgrep<cr>
map <leader>h :h<cr>:CtrlPTag<cr>

map <leader>l <Plug>(operator-duplicate)

nmap <silent> // :CtrlPRelated<cr>

map <leader>rr :R<cr>
map <leader>rm :CtrlPModels<cr>
map <leader>rc :CtrlPControllers<cr>
map <leader>rv :CtrlPViews<cr>
map <leader>rw :CtrlP app/workers<cr>
map <leader>r. :.Rake<cr>
map <leader>r* :Rake<cr>

map <leader>t :TagbarToggle<cr>

nnoremap <leader><leader> <c-^>

nnoremap == gg=G''

nnoremap \ :noh<cr><esc>

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-w>1 :only<cr>

map ~ :cd ~<cr>

map <C-space> *N

nnoremap R cl
