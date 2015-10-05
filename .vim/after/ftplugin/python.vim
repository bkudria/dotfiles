setlocal omnifunc=jedi#completions
autocmd BufWritePost *.py silent! PEP8Radius
