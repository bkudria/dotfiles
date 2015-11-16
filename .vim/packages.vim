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

NeoBundle 'scrooloose/syntastic'
let g:syntastic_check_on_open         = 1
let g:syntastic_aggregate_errors      = 1
let g:syntastic_enable_elixir_checker = 1
let g:syntastic_ruby_checkers         = ['mri']
let g:syntastic_elixir_checkers       = ['elixir']
let g:syntastic_python_python_exec    = '/usr/local/bin/python3'
let g:syntastic_python_checkers       = ['python', 'pyflakes', 'pep8', 'pylint']
let g:syntastic_elixir_checkers       = ['elixir']
let g:syntastic_error_symbol          = 'x'
let g:syntastic_warning_symbol        = '!'
let g:syntastic_style_error_symbol    = '>'
let g:syntastic_style_warning_symbol  = '~'

if exists("b:ismacruby") && b:is_macruby
  let b:syntastic_ruby_checkers = ['macruby']
endif

" NeoBundle 'mhinz/vim-signify'
" let g:signify_sign_overwrite         = 0
" let g:signify_vcs_list               = [ 'git' ]
" let g:signify_sign_add               = '+'
" let g:signify_sign_change            = '±'
" let g:signify_sign_delete            = '_'
" let g:signify_sign_delete_first_line = '‾'

NeoBundle 'fisadev/vim-isort'

NeoBundle 'Shougo/vimproc.vim', { 'build' : { 'mac' : 'make' } }
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'tsukkee/unite-help'
NeoBundle 'Shougo/neomru.vim'
let g:unite_force_overwrite_statusline = 0

NeoBundle 'bling/vim-airline'

set laststatus=2
set noshowmode
let g:airline_extensions = [
      \ 'branch',
      \ 'quickfix',
      \ 'ctrlp',
      \ 'syntastic',
      \ 'tabline',
      \ 'unite',
      \ 'whitespace'
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

let g:airline_theme           = 'solarized'
let g:airline_powerline_fonts = 1
let g:airline_symbols         = {'readonly': '', 'linenr': '', 'modified': '!', }

let g:airline#extensions#whitespace#trailing_format = 'whtspc:%s'

let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#show_tab_type   = 0
let g:airline#extensions#tabline#left_sep        = ''
let g:airline#extensions#tabline#left_alt_sep    = ''

let g:airline#extensions#tabline#formatter = 'custom'

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

NeoBundle 'pantsbuild/vim-pants'
NeoBundle 'edma2/vim-pants'
NeoBundle 'davidhalter/jedi-vim'
let g:jedi#show_call_signatures     = 2
let g:jedi#completions_enabled      = 0
let g:jedi#auto_vim_configuration   = 0
let g:jedi#completions_command      = ''
let g:jedi#goto_command             = ''
let g:jedi#goto_assignments_command = ''
let g:jedi#documentation_command    = ''
let g:jedi#rename_command           = ''
let g:jedi#usages_command           = ''

" let g:EclimCompletionMethod = 'omnifunc'

NeoBundle 'Shougo/neco-syntax'
NeoBundle 'Shougo/neoinclude.vim'
NeoBundle 'Shougo/neocomplete.vim'
let g:acp_enableAtStartup                           = 0
let g:neocomplete#enable_at_startup                 = 1
let g:neocomplete#max_list                          = 10
let g:neocomplete#auto_completion_start_length      = 1
let g:neocomplete#min_keyword_length                = 2
let g:neocomplete#sources#syntax#min_keyword_length = 2
let g:neocomplete#enable_smart_case                 = 1
let g:neocomplete#enable_refresh_always             = 1
let g:neocomplete#enable_auto_select                = 0
let g:neocomplete#force_overwrite_completefunc      = 1

set completeopt-=menuone
set completeopt-=preview
set ofu=syntaxcomplete#Complete
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType ruby,eruby    setlocal omnifunc=rubycomplete#Complete
autocmd FileType java          setlocal omnifunc=eclim#java#complete#CodeComplete
autocmd FileType python        setlocal omnifunc=jedi#completions
autocmd FileType css,scss      setlocal omnifunc=csscomplete#CompleteCSS

if !exists('g:neocomplete#sources')
  let g:neocomplete#sources = {}
endif
let g:neocomplete#sources._ = ['neosnippet', 'omni', 'buffer', 'tag', 'syntax']

if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

if !exists('g:neocomplete#omni_patterns')
  let g:neocomplete#omni_patterns = {}
endif
let g:neocomplete#omni_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'

if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'

if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_omni_input_patterns.ruby   = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplete#force_omni_input_patterns.java   = '\k\.\k*'
let g:neocomplete#force_omni_input_patterns.python =
  \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
let g:neocomplete#force_omni_input_patterns.conf   = '@\w*'

NeoBundle 'Shougo/neosnippet'
let g:neosnippet#disable_runtime_snippets = {
      \   '_' : 1,
      \ }
let g:neosnippet#snippets_directory = '~/.vim/snippets/'
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

" NeoBundle 'aaronjensen/vim-recentcomplete'

NeoBundle 'terryma/vim-multiple-cursors'

NeoBundle 'b4winckler/vim-objc'
NeoBundle 'msanders/cocoa.vim'

"Color Scheme"
NeoBundle 'altercation/vim-colors-solarized'

NeoBundle 'eapache/rainbow_parentheses.vim'
let g:rbpt_max = 20
let g:bold_parentheses = 1
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

let g:rbpt_colorpairs = [
    \ ['yellow',      '#b58900'],
    \ ['brown',       '#cb4b16'],
    \ ['red',         '#dc322f'],
    \ ['magenta',     '#d33682'],
    \ ['darkmagenta', '#6c71c4'],
    \ ['blue',        '#268bd2'],
    \ ['cyan',        '#2aa198'],
    \ ['green',       '#859900'],
    \ ]

NeoBundle 'rizzatti/funcoo.vim'

NeoBundle 'justinmk/vim-sneak'
hi! link SneakPluginTarget IncSearch

NeoBundle 'thinca/vim-localrc'

NeoBundle 'gcmt/tube.vim'
let g:tube_terminal = "iterm"

NeoBundle 'dhruvasagar/vim-vinegar'

NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'geekjuice/vim-mocha'

NeoBundle 'mustache/vim-mustache-handlebars'

NeoBundle 'groenewege/vim-less'

NeoBundle 'tfnico/vim-gradle'
NeoBundle 'vim-scripts/groovy.vim'

NeoBundle 'Yggdroot/indentLine'
let g:indentLine_char = '│'
let g:indentLine_color_gui = '#073642'
let g:indentLine_noConcealCursor = 1

NeoBundle 'airblade/vim-rooter'
let g:rooter_disable_map  = 1
let g:rooter_silent_chdir = 1
let g:rooter_patterns     = ['.git/', 'build.gradle']

NeoBundle 'jaxbot/semantic-highlight.vim'
let g:semanticGUIColors = [
      \ '#b58900',
      \ '#cb4b16',
      \ '#dc322f',
      \ '#d33682',
      \ '#6c71c4',
      \ '#268bd2',
      \ '#2aa198',
      \ '#859900'
      \ ]

NeoBundle 'bkudria/ctrlp-related'

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

NeoBundle 'qpkorr/vim-renamer'
let g:RenamerSupportColonWToRename = 1

" NeoBundle 'JazzCore/ctrlp-cmatcher', {'build': {'mac': 'CFLAGS=-Qunused-arguments CPPFLAGS=-Qunused-arguments ./install.sh'}}
" let g:ctrlp_match_func = {'match' : 'matcher#cmatch'}

" NeoBundle 'bkudria/vim-hardy'
NeoBundle 'sophacles/vim-processing'

NeoBundle 'bkudria/vim-pep8radius'

NeoBundle 'rhysd/conflict-marker.vim'

NeoBundle 'Glench/Vim-Jinja2-Syntax'

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
