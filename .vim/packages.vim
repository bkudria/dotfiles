filetype off

if has('vim_starting')
  if &compatible
    set nocompatible
  endif
  set rtp+=~/.vim/bundle/neobundle.vim/
end
call neobundle#begin(expand('~/.vim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'iurifq/ctrlp-rails.vim', {'depends' : 'ctrlpvim/ctrlp.vim' }
let g:ctrlp_max_height          = 40
let g:ctrlp_max_files           = 0
let g:ctrlp_open_new_file       = 'r'
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_max_depth           = 30
let g:ctrlp_max_files           = 400000
let g:ctrlp_open_single_match   = ['related', 'tags']
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$\|\.yardoc\|public\/images\|public\/system\|data\|log\|tmp\|node_modules\|.coffee$',
  \ 'file': '\.exe$\|\.so$\|\.dat$\|\.gitkeep$'
  \ }
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files -cmo --exclude-standard | ( [ -f .ctrlp_ignore ] && grep -vf .ctrlp_ignore || cat ) | sort | uniq'],
    \ },
  \ 'fallback': 'find %s -type f'
  \ }
let g:ctrlp_open_func = { 'dirs': 'CtrlPCWD'}
function! CtrlPCWD(action, line)
  call ctrlp#dir#accept('t', a:line)
endfunction

NeoBundle 'mhinz/vim-sayonara'

NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'gilligan/textobj-gitgutter'

NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'jmcantrell/vim-virtualenv'
let g:jedi#show_call_signatures     = 2
let g:jedi#completions_enabled      = 0
let g:jedi#popup_on_dot             = 0
let g:jedi#auto_vim_configuration   = 0
NeoBundleLazy 'lambdalisue/vim-pyenv', {
        \ 'depends': ['davidhalter/jedi-vim'],
        \ 'autoload': {
        \   'filetypes': ['python', 'python3'],
        \ }}

NeoBundle 'vim-syntastic/syntastic'
let g:syntastic_check_on_open         = 1
let g:syntastic_aggregate_errors      = 1
let g:syntastic_enable_elixir_checker = 1
let g:syntastic_ruby_checkers         = ['mri']
let g:syntastic_elixir_checkers       = ['elixir']
let g:syntastic_python_python_exec    = '/usr/local/bin/python3'
" let g:syntastic_python_pylint_exec = system('pyenv which pylint')
let g:syntastic_python_pylint_exec = '/Users/bkudria/.pyenv/shims/pylint'
let g:syntastic_python_checkers       = ['python', 'pyflakes', 'pep8']
let g:syntastic_javascript_checkers   = ['eslint']
let g:syntastic_error_symbol          = 'x'
let g:syntastic_warning_symbol        = '!'
let g:syntastic_style_error_symbol    = '>'
let g:syntastic_style_warning_symbol  = '~'

if exists("b:ismacruby") && b:is_macruby
  let b:syntastic_ruby_checkers = ['macruby']
endif

NeoBundle 'mtscout6/syntastic-local-eslint.vim'

NeoBundle 'fisadev/vim-isort'

NeoBundle 'Shougo/vimproc.vim', { 'build' : { 'mac' : 'make' } }
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'tsukkee/unite-help'
NeoBundle 'Shougo/neomru.vim'
let g:unite_force_overwrite_statusline = 0

NeoBundle 'vim-airline/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'

set laststatus=2
set noshowmode
let g:airline_extensions = [
      \ 'branch',
      \ 'quickfix',
      \ 'ctrlp',
      \ 'syntastic',
      \ 'tabline',
      \ 'unite',
      \ 'whitespace',
      \ 'virtualenv'
      \ ]

if exists("airline#extensions#eclim#init")
  g:airline_extensions += ['eclim']
endif

let g:airline_mode_map = {
      \ '__' : '-',
      \ 'n'  : 'N',
      \ 'i'  : 'I',
      \ 'R'  : 'R',
      \ 'c'  : 'C',
      \ 'v'  : 'V ',
      \ 'V'  : 'LV',
      \ '' : 'BV',
      \ 's'  : 'S',
      \ 'S'  : 'LS',
      \ '' : 'BS',
      \ }

function! AirlineInit()
  let g:airline_section_a = airline#section#create(['mode'])
  let g:airline_section_b = airline#section#create(['branch'])
  let g:airline_section_c = airline#section#create(['file'])
endfunction
autocmd User AirlineAfterInit call AirlineInit()

let g:airline_theme           = 'gruvbox'
let g:airline_powerline_fonts = 1
let g:airline_symbols         = {'readonly': '', 'linenr': '', 'modified': '!', }

let g:airline#extensions#whitespace#trailing_format = 'whtspc:%s'

let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#show_tab_type   = 0
let g:airline#extensions#tabline#left_sep        = ''
let g:airline#extensions#tabline#left_alt_sep    = ''

let g:airline#extensions#tabline#formatter = 'custom'

NeoBundle 'jreybert/vimagit'
let g:magit_show_help=0
let g:magit_default_show_all_files=1
let g:magit_default_fold_level=2

NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-rhubarb'
NeoBundle 'gregsexton/gitv'
NeoBundle 'tpope/vim-rails'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'ck3g/vim-change-hash-syntax'
NeoBundle 'jgdavey/vim-blockle'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-bundler'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-sleuth'
NeoBundle 'tpope/vim-sensible'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'tpope/vim-unimpaired'
set undofile
set undodir^=~/.vim/undo

NeoBundle 'tpope/vim-haml'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim'

NeoBundle 'tpope/vim-abolish'
" Replace defined :s
cabbrev s <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'S' : 's')<CR>

NeoBundle 'tpope/vim-rbenv'

NeoBundle 'Raimondi/delimitMate'
au FileType elixir let b:delimitMate_nesting_quotes = ['"']


NeoBundle 'majutsushi/tagbar'
let g:tagbar_compact   = 1
let g:tagbar_indent    = 1
let g:tagbar_autofocus = 1

NeoBundle 'godlygeek/tabular'
NeoBundle 'ecomba/vim-ruby-refactoring'
NeoBundle 'vim-scripts/Rename'

NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-textobj-line'
NeoBundle 'nelstrom/vim-textobj-rubyblock'
NeoBundle 'lucapette/vim-textobj-underscore'
NeoBundle 'bps/vim-textobj-python'
NeoBundle 'bootleq/vim-textobj-rubysymbol'
NeoBundle 'RyanMcG/vim-textobj-dash'
NeoBundle 'vim-scripts/argtextobj.vim'
NeoBundle 'michaeljsmith/vim-indent-object'
NeoBundle 'Julian/vim-textobj-brace'
NeoBundle 'kana/vim-textobj-syntax'
NeoBundle 'killphi/vim-textobj-signify-hunk'
NeoBundle 'kana/vim-textobj-lastpat'
NeoBundle 'Julian/vim-textobj-variable-segment'
NeoBundle 'whatyouhide/vim-textobj-xmlattr'
NeoBundle 'saaguero/vim-textobj-pastedtext'

NeoBundle 'terryma/vim-expand-region'
let g:expand_region_text_objects = {
      \ 'iw'  :0,
      \ 'i"'  :0,
      \ 'i''' :0,
      \ 'i]'  :1,
      \ 'ib'  :1,
      \ 'iB'  :1,
      \ 'a]'  :1,
      \ 'ab'  :1,
      \ 'aB'  :1,
      \ 'ii'  :0,
      \ 'ai'  :0
      \ }

NeoBundle 'mattn/webapi-vim'
NeoBundle 'mattn/gist-vim'
let g:gist_clip_command     = 'pbcopy'
let g:gist_detect_filetype  = 1
let g:gist_post_private     = 1
let g:gist_get_multiplefile = 1

NeoBundle 'kana/vim-operator-user'
NeoBundle 'kana/vim-operator-replace', {'depends' : 'kana/vim-operator-user' }

NeoBundle 'jeffkreeftmeijer/vim-numbertoggle'
NeoBundle 'grep.vim'
let Grep_Xargs_Options = '-0 -P 10'
let Grep_Default_Options = '-S -m 1000'
" TODO Generalize for project search
let Grep_Skip_Dirs = '.git log node_modules .coffee .pants.d'
let Grep_Path = '/usr/local/bin/ag'
let Grep_Default_Filelist = '*'

NeoBundle 'Shougo/echodoc.vim'
let g:echodoc_enable_at_startup = 1

NeoBundle 'marijnh/tern_for_vim', { 'build': { 'mac': 'npm install' } }
NeoBundle 'jelera/vim-javascript-syntax'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'mxw/vim-jsx'
NeoBundle 'mvolkmann/vim-js-arrow-function'


let g:neobundle#install_process_timeout = 1500
NeoBundle 'Valloric/YouCompleteMe', {
     \ 'build' : {
     \     'unix' : './install.sh --tern-completer',
     \    }
     \ }
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_seed_identifiers_with_syntax = 1

set completeopt-=menuone
set completeopt-=preview

NeoBundle 'mattn/emmet-vim'
let g:user_emmet_leader_key='<m-space>'

NeoBundle 'terryma/vim-multiple-cursors'

NeoBundle 'b4winckler/vim-objc'
NeoBundle 'msanders/cocoa.vim'

"Color Scheme"
" NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'morhetz/gruvbox'
let g:gruvbox_contrast_dark="soft"
let g:gruvbox_contrast_dark="soft"
let g:gruvbox_sign_column="bg0"
let g:gruvbox_italicize_strings=1
let g:gruvbox_invert_selection=0
let g:gruvbox_improved_warnings=1

NeoBundle 'eapache/rainbow_parentheses.vim'
let g:rbpt_max = 20
let g:bold_parentheses = 1
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

NeoBundle 'rizzatti/funcoo.vim'

NeoBundle 'justinmk/vim-sneak'
hi! link SneakPluginTarget IncSearch

NeoBundle 'thinca/vim-localrc'

NeoBundle 'gcmt/tube.vim'
let g:tube_terminal = "iterm"

NeoBundle 'justinmk/vim-dirvish'
augroup dirvish_events
  autocmd!

  " Enable :Gstatus and friends.
  autocmd FileType dirvish call fugitive#detect(@%)

  autocmd FileType dirvish keeppatterns g@\v/\.[^\/]+/?$@d
augroup END

NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'geekjuice/vim-mocha'

NeoBundle 'mustache/vim-mustache-handlebars'

NeoBundle 'groenewege/vim-less'

NeoBundle 'tfnico/vim-gradle'
NeoBundle 'vim-scripts/groovy.vim'

NeoBundle 'Yggdroot/indentLine'
let g:indentLine_char = '│'
" let g:indentLine_color_gui = '#073642'
let g:indentLine_noConcealCursor = 1

NeoBundle 'airblade/vim-rooter'
let g:rooter_disable_map  = 1
let g:rooter_silent_chdir = 1
let g:rooter_patterns     = ['.root', '.git/', 'build.gradle']

NeoBundle 'jaxbot/semantic-highlight.vim'

NeoBundle 'haya14busa/incsearch.vim'
let g:incsearch#auto_nohlsearch = 1

NeoBundle 'junkblocker/patchreview-vim'

NeoBundle 'gregsexton/MatchTag'

NeoBundle 'vasconcelloslf/vim-interestingwords'
let g:interestingWordsGUIColors = [
      \ '#b58900',
      \ '#cb4b16',
      \ '#dc322f',
      \ '#d33682',
      \ '#6c71c4',
      \ '#268bd2',
      \ '#2aa198',
      \ '#859900'
      \ ]

NeoBundle 'elixir-lang/vim-elixir'
NeoBundle 'mattreduce/vim-mix'
NeoBundle 'ryanss/vim-hackernews'

NeoBundle 'hdima/python-syntax'
NeoBundle 'vim-scripts/django.vim'
NeoBundle 'jmcomets/vim-pony'

NeoBundle 'qpkorr/vim-renamer'
let g:RenamerSupportColonWToRename = 1

" NeoBundle 'bkudria/vim-hardy'
NeoBundle 'sophacles/vim-processing'

" NeoBundle 'bkudria/vim-pep8radius'

NeoBundle 'rhysd/conflict-marker.vim'

NeoBundle 'Glench/Vim-Jinja2-Syntax'

NeoBundle 'bwmcadams/vim-deckset'

NeoBundle 'joeytwiddle/sexy_scroller.vim'

NeoBundle 'vim-scripts/po.vim--gray'

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
