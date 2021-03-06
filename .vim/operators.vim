call operator#user#define('exec', 'ExecuteOperator')
function! ExecuteOperator(motion_wiseness)
  let txt = MotionText(a:motion_wiseness)
  let txt = substitute(txt, "[\<C-M>\<C-@>]$", '', '')
  execute txt
endfunction

call operator#user#define('echo', 'EchoOperator')
function! EchoOperator(motion_wiseness)
  echo MotionText(a:motion_wiseness)
endfunction

call operator#user#define('duplicate', 'DuplicateOperator')
function! DuplicateOperator(motion_wiseness)
  let @t =  MotionText(a:motion_wiseness)
  normal `]
  normal "tp
endfunction

call operator#user#define('dash',       'DashOperator', 'call SetDashKeyword("")')
call operator#user#define('dash-ruby',  'DashOperator', 'call SetDashKeyword("ruby")')
call operator#user#define('dash-rails', 'DashOperator', 'call SetDashKeyword("rails")')
let s:dash_keyword = ''
function! SetDashKeyword(keyword)
  let s:dash_keyword = a:keyword
endfunction
function! DashOperator(motion_wiseness)
  execute 'Dash' . (s:dash_keyword == '' ? '! ' : ' ') . MotionText(a:motion_wiseness) . ' ' . s:dash_keyword
endfunction

function! MotionText(motion_wiseness)
  let old_value = @"
  let v = operator#user#visual_command_from_wise_name(a:motion_wiseness)
  silent execute 'normal!' . '`[' . v . '`]""y'
  let return_value = @"
  let @" = old_value
  return return_value
endfunction

call operator#user#define_ex_command('coffee-compile', 'CoffeeCompile')

call operator#user#define('jumptag', 'JumpTagOperator')
function! JumpTagOperator(motion_wiseness)
  echom MotionText(a:motion_wiseness)
  call BrowseOrJumpTag(MotionText(a:motion_wiseness))
endfunction

call operator#user#define('expand-js-obj', 'ExpandJSObjOperator')
function! ExpandJSObjOperator(motion_wiseness)
  let v = operator#user#visual_command_from_wise_name(a:motion_wiseness)
  let @e = ExpandJSObj(MotionText(a:motion_wiseness))
  execute 'normal!' . '`[' . v . '`]'
  execute 'normal "e-'
endfunction
