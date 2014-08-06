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

