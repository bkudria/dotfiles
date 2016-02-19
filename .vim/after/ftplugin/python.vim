
setlocal omnifunc=jedi#completions
" autocmd BufWritePost *.py silent! PEP8Radius

nmap <buffer> <localleader>jt :call jedi#goto()<cr>

" function! s:reset_syntastic_pylint_exec()
"   let g:syntastic_python_pylint_exec = system('pyenv which pylint')
" endfunction

if jedi#init_python()
  function! s:jedi_auto_force_py_version() abort
    let major_version = pyenv#python#get_internal_major_version()
    call jedi#force_py_version(major_version)
  endfunction
  augroup vim-pyenv-custom-augroup
    autocmd! *
    " autocmd User vim-pyenv-activate-post   call s:reset_syntastic_pylint_exec()
    autocmd User vim-pyenv-activate-post   call s:jedi_auto_force_py_version()

    " autocmd User vim-pyenv-deactivate-post call s:reset_syntastic_pylint_exec()
    autocmd User vim-pyenv-deactivate-post call s:jedi_auto_force_py_version()
  augroup END
endif

PyenvActivate 3.4.3
