call operator#user#define('exec', 'ExecuteOperator')
function! ExecuteOperator(motion_wiseness)
  execute MotionText(a:motion_wiseness)
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
