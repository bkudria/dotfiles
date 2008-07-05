set nocompatible	" enable non-compatible options
set mouse=a				" mouse support
set nu						" line numbering
set wrap					" wrap long lines
set bg=dark				" dark background
set stal=2				" tab line
set mousef				" mouse focus
set mousem=popup	" popup on right click
set gfn=Consolas,Terminus,mono	" fonts
set smd						" show mode
set vb						" visual bell
set ts=2					" <tab> is 2 wide
set sw=2					" 2 spaces for indent
set nosta					" turn off smarttab
set sts=0					" insert no spaces
set noet					" don't expand <tab>
set ai						" auto-indent
set si						" smart indent
set ml						" use modelines
set ar						" re-read modified file automatically
set fs						" sync file to disk after writing
set hi=10000000		" keep a lot of history
set lpl						" load plugins
set debug=msg			" show errors
set showcmd				" show incomplete commands


" Turn on colors if they are available
if &t_Co > 2 || has("gui_running")
	syntax on
	set hlsearch
endif

" attempt to guess file, load appropriate plugin, and use appropriate indent
filetype plugin indent on