let g:mapleader="\<space>"
let g:maplocalleader="\<bslash>"

call unite#custom#profile('default', 'context', {
\       'no_resize'   : 1,
\       'no_split'    : 1,
\       'prompt'      : '> '
\ })

call unite#custom#profile('source/cwd', 'context', {
\       'default_action'   : 'cd',
\ })

call unite#custom#profile('source/outline', 'context', {
\       'winwidth'         : 30,
\       'vertical'         : 1,
\       'vertical_preview' : 1,
\       'auto_preview'     : 1
\ })

call unite#custom#profile('source/line', 'context', {
\       'custom_line_enable_highlight' : 1,
\       'start_insert'                 : 1
\ })

let g:unite_source_alias_aliases = {
      \   'cwd' : {
      \     'source' : 'directory',
      \   },
      \ }

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_selecta'])

let g:unite_source_tag_show_kind        = 0
let g:unite_source_tag_max_name_length  = 30
let g:unite_source_tag_max_fname_length = 50

hi link uniteInputPrompt Special

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  map  <buffer> <D-w> <Plug>(unite_exit)
  nmap <buffer> q     <Plug>(unite_exit)
  nmap <buffer> <esc> <Plug>(unite_exit)
  nmap <buffer> <F1>  <Plug>(unite_quick_help)
  nmap <buffer> <F5>  <Plug>(unite_redraw)
  imap <buffer> <Tab> <Plug>(unite_complete)
endfunction

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

  macmenu &File.Save key=<nop>
  nmap <D-s> <nop>
  imap <expr> <D-s> pumvisible() ? neocomplete#close_popup() . "<ESC><D-s>" : "<ESC><D-s>"

  macmenu &File.Close key=<nop>
  nmap <D-w> :Sayonara<cr>
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
map <F2> :Unite buffer    -start-insert<cr>
map <F3> :Unite file_mru  -start-insert<cr>
map <F4> :Unite tag -start-insert<cr>
map <F5> :e!<cr>
map <F6> :SemanticHighlightToggle<cr>
map              <F7> :Unite outline -toggle<cr>
map <localleader><F7> :Unite outline -toggle -immediately -start-insert<cr>

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
let g:blockle_mapping = '<Leader>bt'

map <leader>cl :ccl<cr>
map              <leader>cd :Unite cwd:~/Code -start-insert<cr>
map <localleader><leader>cd :Unite cwd:~<cr>

nmap <Leader>cs <Plug>GitGutterStageHunk
nmap <Leader>cr <Plug>GitGutterRevertHunk
nmap <Leader>cv <Plug>GitGutterPreviewHunk
omap ic <Plug>(textobj-gitgutter-i)
xmap ic <Plug>(textobj-gitgutter-i)

map %% :CopyRelativeFilePath<cr>
map <leader>%% :CopyAbsoluteFilePath<cr>

map <leader>d   :Unite grep/git:/::TODO<cr>

map <leader>e <Plug>(operator-exec)

map <leader>f  :Unite grep/git:/::<cr>
map <leader>fl :Unite line<cr>
map <leader>gd :Unite grep/git:::<cr>

map <leader>gs :Gstatus<cr>
map <leader>gr :Gread<cr>
map <leader>gw :Gwrite<cr>
map <leader>gcc :Gcommit<cr>
map <leader>gcm :Gcommit -m ""<Left>
map <leader>gpl :Git pull<cr>
map <leader>gps :Git push<cr>
map <leader>gom :CtrlPModified<CR>
map <leader>gob :CtrlPBranch<CR>
map <leader>gbl :Gblame<CR>

map <leader>h :Unite help -resume -no-split -start-insert -input=<cr>

map <leader>l <Plug>(operator-duplicate)

nmap <silent> // :CtrlPRelated<cr>

map <leader>o         :Unite file_rec/git:-cmo:--exclude-standard -start-insert<cr>
map <leader><leader>o :Unite file_rec/git:-cmo:--exclude-standard -start-insert<cr>

map <leader>rr :R<cr>
map <leader>rm :CtrlPModels<cr>
map <leader>rc :CtrlPControllers<cr>
map <leader>rv :CtrlPViews<cr>
map <leader>rw :CtrlP app/workers<cr>
map <leader>r. :.Rake<cr>
map <leader>r* :Rake<cr>

nmap <leader>s :write<cr>

nmap <leader>t <Plug>(operator-jumptag)
vmap <leader>t :BrowseOrJumpTag <c-r>=Vselection()<cr>

nmap <leader>u :UniteResume<cr>

map <leader>w :Sayonara<cr>
map <leader>x :Sayonara<cr>
map <leader>q :Sayonara<cr>

nnoremap <localleader><localleader> <c-^>

nmap [u :UnitePrevious<cr>
nmap ]u :UniteNext<cr>

nnoremap == gg=G''

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-w>1 :only<cr>

map ~ :cd ~<cr>

nnoremap <silent> <C-space> :call InterestingWords('n')<cr>
nnoremap <silent> \         :call UncolorAllWords()<cr>:nohl<cr>
nnoremap <silent> n         :call WordNavigation(1)<cr>
nnoremap <silent> N         :call WordNavigation(0)<cr>

nnoremap R cl

nmap Y yil
