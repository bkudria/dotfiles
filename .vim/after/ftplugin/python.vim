setlocal omnifunc=jedi#completions
" autocmd BufWritePost *.py silent! PEP8Radius

nmap <buffer> <localleader>jt :call jedi#goto()<cr>
