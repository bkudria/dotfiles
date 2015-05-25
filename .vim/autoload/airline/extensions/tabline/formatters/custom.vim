function! s:fnameext(bufnr)
  return fnamemodify(bufname(a:bufnr), ':t')
endfunction

function! s:fname(bufnr)
  return fnamemodify(bufname(a:bufnr), ':t:r')
endfunction

function! s:sfname(bufnr)
  let pure_filename   = fnamemodify(bufname(a:bufnr), ':t:r')
  if pure_filename == ""
    return "[no name]"
  endif
  let parts_pattern   = '[a-z][A-Z]\@=\zs\|-\|_'
  let filename_parts  = split(pure_filename, parts_pattern)
  let leading_letters = map(filename_parts[:-2], 'strpart(v:val, 0, 1)')
  return join(leading_letters, '') . filename_parts[-1]
endfunction

function! airline#extensions#tabline#formatters#custom#format(bufnr, buffers)
  let width = &columns - 10

  let fnameexts = join(map(copy(a:buffers), '" n" . s:fnameext(v:val) . " "'), '')
  if strlen(fnameexts) < width
    return s:fnameext(a:bufnr)
  endif

  let fnames = join(map(copy(a:buffers), '" n" . s:fname(v:val) . " "'), '')
  if strlen(fnames) < width
    return s:fname(a:bufnr)
  endif

  return s:sfname(a:bufnr)
endfunction
