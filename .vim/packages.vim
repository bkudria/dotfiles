filetype off

if has('vim_starting')
  set rtp+=~/.vim/bundle/neobundle.vim/
end
call neobundle#begin(expand('~/.vim/bundle'))

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'mac'  : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

NeoBundle 'kien/ctrlp.vim'
NeoBundle 'iurifq/ctrlp-rails.vim', {'depends' : 'kien/ctrlp.vim' }
let g:ctrlp_max_height          = 25
let g:ctrlp_max_files           = 0
let g:ctrlp_open_new_file       = 'r'
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_max_depth           = 20
let g:ctrlp_max_files           = 200000
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$\|\.yardoc\|public\/images\|public\/system\|data\|log\|tmp\|node_modules$',
  \ 'file': '\.exe$\|\.so$\|\.dat$\|\.gitkeep$'
  \ }
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files'],
    \ },
  \ 'fallback': 'find %s -type f'
  \ }
let g:ctrlp_open_func = { 'dirs': 'CtrlPCWD'}
function! CtrlPCWD(action, line)
  call ctrlp#dir#accept('t', a:line)
endfunction

NeoBundle 'aaronjensen/vim-command-w', {'depends' : 'vim-scripts/bufkill.vim'}

NeoBundle 'fisadev/vim-ctrlp-cmdpalette'

NeoBundle 'scrooloose/syntastic'
let g:syntastic_check_on_open        = 1
let g:syntastic_aggregate_errors     = 1
let g:syntastic_ruby_checkers        = ['mri']
let g:syntastic_error_symbol         = 'x'
let g:syntastic_warning_symbol       = '!'
let g:syntastic_style_error_symbol   = '>'
let g:syntastic_style_warning_symbol = '~'
if exists("b:ismacruby") && b:is_macruby
  let b:syntastic_ruby_checkers = ['macruby']
endif

NeoBundle 'bling/vim-airline'
set noshowmode
let g:airline_theme                             = 'solarized'
let g:airline_powerline_fonts                   = 1
let g:airline_symbols                           = {'readonly': '', 'linenr': '', 'modified': '!', }
let g:airline#extensions#hunks#non_zero_only    = 0

NeoBundle 'mhinz/vim-signify'
let g:signify_sign_overwrite         = 0
let g:signify_vcs_list               = [ 'git' ]
let g:signify_sign_add               = '+'
let g:signify_sign_change            = '±'
let g:signify_sign_delete            = '_'
let g:signify_sign_delete_first_line = '‾'

NeoBundle 'tpope/vim-fugitive'
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
NeoBundle 'tpope/vim-haml'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'tpope/vim-unimpaired'
set undodir^=~/.vim/undo

NeoBundle 'tpope/vim-abolish'
" Replace defined :s
cabbrev s <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'S' : 's')<CR>

NeoBundle 'tpope/vim-rbenv'

NeoBundle 'Raimondi/delimitMate'
NeoBundle 'spiiph/vim-space'
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
NeoBundle 'coderifous/textobj-word-column.vim'
NeoBundle 'bootleq/vim-textobj-rubysymbol'
NeoBundle 'RyanMcG/vim-textobj-dash'
NeoBundle 'vim-scripts/argtextobj.vim'
NeoBundle 'kana/vim-textobj-indent'
NeoBundle 'Julian/vim-textobj-brace'
NeoBundle 'kana/vim-textobj-syntax'

NeoBundle 'bkad/CamelCaseMotion'
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
let Grep_Skip_Dirs = '.git log'
let Grep_Path = '/usr/local/bin/ag'
let Grep_Default_Filelist = '*.coffee'

NeoBundle 'ervandew/supertab'
let g:SuperTabDefaultCompletionType = "<c-x><c-u>"

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
set ofu=syntaxcomplete#Complete
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType ruby,eruby    setlocal omnifunc=rubycomplete#Complete

if !exists('g:neocomplete#sources')
  let g:neocomplete#sources = {}
endif
let g:neocomplete#sources._ = ['buffer', 'tag', 'syntax']

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
let g:neocomplete#force_omni_input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'

NeoBundle 'terryma/vim-multiple-cursors'

NeoBundle 'b4winckler/vim-objc'
NeoBundle 'msanders/cocoa.vim'

"Color Scheme"
NeoBundle 'altercation/vim-colors-solarized'

NeoBundle 'kien/rainbow_parentheses.vim'
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

NeoBundle 'rizzatti/funcoo.vim'

NeoBundle 'justinmk/vim-sneak'

NeoBundle 'thinca/vim-localrc'

NeoBundle 'gcmt/tube.vim'
let g:tube_terminal = "iterm"

NeoBundle 'Xuyuanp/git-nerdtree'
NeoBundle 'dhruvasagar/vim-vinegar'

NeoBundle 'kchmck/vim-coffee-script'

NeoBundle 'tfnico/vim-gradle'
NeoBundle 'vim-scripts/groovy.vim'

NeoBundle 'Yggdroot/indentLine'
let g:indentLine_char = '│'
let g:indentLine_color_gui = '#073642'
let g:indentLine_noConcealCursor = 1

NeoBundle 'flomotlik/vim-livereload', {
      \ 'build' : {
      \     'mac'  : 'rake',
      \     'unix' : 'rake',
      \    },
      \ }

" NeoBundle 'amiorin/ctrlp-z.git'
" NeoBundle 'phalkunz/ctrlp-related'

NeoBundle 'jasoncodes/ctrlp-modified.vim'

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
