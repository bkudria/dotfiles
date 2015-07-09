let mapleader="\<space>"
let g:mapleader="\<space>"

map <up>    <nop>
map <down>  <nop>
map <left>  <nop>
map <right> <nop>

nmap <D-Left>  :bp<cr>
nmap <D-Right> :bn<cr>

nmap <D-1> <Plug>AirlineSelectTab1
nmap <D-2> <Plug>AirlineSelectTab2
nmap <D-3> <Plug>AirlineSelectTab3
nmap <D-4> <Plug>AirlineSelectTab4
nmap <D-5> <Plug>AirlineSelectTab5
nmap <D-6> <Plug>AirlineSelectTab6
nmap <D-7> <Plug>AirlineSelectTab7
nmap <D-8> <Plug>AirlineSelectTab8
nmap <D-9> <Plug>AirlineSelectTab9

if has("gui_macvim") && has("gui_running")
  macmenu &File.Open\.\.\. key=<nop>
  map <D-o> :CtrlPCurWD<CR>

  macmenu &File.Save key=<nop>
  nmap <D-s> :write<CR>
  imap <expr> <D-s> pumvisible() ? neocomplete#close_popup() . "<ESC><D-s>" : "<ESC><D-s>"

  macmenu &File.Close key=<nop>
  nmap <D-w> :Sayonara<cr>

  macmenu &Edit.Find.Find\.\.\. key=<nop>
  map <D-f> /
endif

inoremap <Space> <C-g>u<Space>

inoremap <expr> <ESC>  pumvisible() ? neocomplete#cancel_popup() : "\<ESC>"

imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ? "\<C-n>" : "\<TAB>")
vmap <TAB> <Plug>(neosnippet_expand_target)
imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""

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

nmap << <ii
nmap >> >ii

" Ctrl-j/k deletes blank line below/above, and Alt-j/k inserts.
nnoremap <silent><D-Up>     m`:silent -g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent><D-S-Up>   m`:silent +g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent><D-Down>   :set paste<CR>m`O<Esc>``:set nopaste<CR>
nnoremap <silent><D-S-Down> :set paste<CR>m`o<Esc>``:set nopaste<CR>

map <F1> <leader>h
map <F2> :CtrlPBuffer<cr>
map <F3> :CtrlPMRUFiles<cr>
map <F4> :CtrlPTag<cr>
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
nmap <leader>a:  vaI:Tabularize /^[^:]*\zs:/l1<cr>
vmap <leader>a:  :Tabularize /^[^:]*\zs:/l1<cr>
map <leader>a,  :Tabularize /,\zs<cr>
map <leader>a=  :Tabularize /=<cr>
map <leader>a=> :Tabularize /=><cr>
map <leader>a-> :Tabularize /-><cr>

map <leader>bd  :e .<cr>

map <leader>c <plug>(operator-coffee-compile)

map <leader>cl :ccl<cr>
map <leader>cd :CtrlPDir ~<cr>

map %% :CopyRelativeFilePath<cr>
map <leader>%% :CopyAbsoluteFilePath<cr>

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

map <leader>o <Plug>(operator-open)

nmap <silent> // :CtrlPRelated<cr>

map <leader>rr :R<cr>
map <leader>rm :CtrlPModels<cr>
map <leader>rc :CtrlPControllers<cr>
map <leader>rv :CtrlPViews<cr>
map <leader>rw :CtrlP app/workers<cr>
map <leader>r. :.Rake<cr>
map <leader>r* :Rake<cr>

nmap <leader>t <Plug>(operator-jumptag)
vmap <leader>t :BrowseOrJumpTag <c-r>=Vselection()<cr>

nnoremap <leader><leader> <c-^>

nnoremap == gg=G''

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-w>1 :only<cr>

map ~ :cd ~<cr>

nnoremap <silent> <C-space> :call InterestingWords('n')<cr>
nnoremap <silent> \         :call UncolorAllWords()<cr>
nnoremap <silent> n         :call WordNavigation(1)<cr>
nnoremap <silent> N         :call WordNavigation(0)<cr>

nnoremap R cl
