function! Vselection()
  try
    let a_save = @a
    normal! gv"ay
    return @a
  finally
    let @a = a_save
  endtry
endfunction

function! Cword()
  expand('<cword>')
endfunction

function! SignifyToggleGit()
  if exists('g:signify_diffoptions.git')
    unlet g:signify_diffoptions.git
  else
    let g:signify_diffoptions.git = 'develop...'
  endif
  exec "SignifyToggle"
  exec "SignifyToggle"
endfun
command! SignifyToggleGit call SignifyToggleGit()

function! ReviewBranch(branch)
  silent exec "Git fetch"
  exec "DiffReview git diff origin/develop...origin/" . a:branch . " --"
endfun

function! Strip(input_string)
  return substitute(a:input_string, '^\s*\(.\{-}\)\s*$', '\1', '')
endfun

function! GitRemoteBranches(start, cmd, cursor)
  return map(filter(split(system('git branch -r --list'), '\n')[1:-1], 'v:val =~ "' . a:start . '"'), 'substitute(Strip(v:val), "origin/", "", "")')
endfun

command! -nargs=1 -complete=custom,GitRemoteBranches ReviewBranch call ReviewBranch(<q-args>)

function! CopyFullFilePath()
  let @*=expand('%:p')
endfun
command! CopyFullFilePath call CopyFullFilePath()

function! CopyRelativeFilePath()
  let @*=expand('%')
endfun
command! CopyRelativeFilePath call CopyRelativeFilePath()
