" Remap ';' to ':', so I don't have to hold down the shift key.
map ; :

" Append to end of file, in one keystroke.
:nmap ^G Go

" Insert a single character.
:nnoremap s :exec "normal i".nr2char(getchar())."\e"<CR>

" BROKEN: Append a single character.
" :nnoremap S :exec "normal a".nr2char(getchar())."\e"<CR>

" set autoindent
" set filetype plugin indent on
" set smartindent
" set tabstop=4
" set shiftwidth=4
" set expandtab

