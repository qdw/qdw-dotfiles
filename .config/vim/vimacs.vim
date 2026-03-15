""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" emacsish vim plugin: bring core Emacs keybindings to vim, 
" and make (most of) them work regardless of mode.
"
" Motivation: sometimes I want vim's unmatched low display latency,
" but I still want the Emacs commands that are burned into my muscle memory.
"
" Caveats:
"
" - This plugin clobbers some common vim keybindings like <C-f>.
"
" - Our goal is to map Emacs keys to the *analogous* vim acions,
"   *not to replicate* Emacs functionality exactly. For instance,
"   if you press <C-f> at the end of a line, emacsish does not advance
"   to the next line, while, in Emacs, it does advance.
"   
"   Likewise, <M-f> moves forward according to vim's definition of a word,
"   using vim's built-in <w> command - it does not move forward using
"   the emacs definition of a word.
"
" - FIXME: this is tested in gvim only (specifically, in Macvim).
"   It has not been tested in terminal vim, and it may not handle
"   terminal escapes properly.
" 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""":

"""" Avoid double‑loading this plugin.

if exists("g:loaded_emacsish")
  finish
endif
let g:loaded_emacsish = 1

"""" Backspace

" - Use a single backspace keypress to delete one level of indentation.
" - Allow backspacing all the way up into the preceding line.
" - Don't stop backspacing at to the char where you entered insert mode.

set backspace=indent,eol,nostop

"""" Tab key: indent the line we're on, regardless of the column we're on.

set expandtab
nnoremap <Tab> ==
inoremap <Tab> <Esc>==i

augroup EmacsishTabs
  autocmd!
  autocmd BufWinEnter,InsertLeave * match ExtraTab /\t/
augroup END

"""" Appearance: cursor

" Non-blinking block cursor in all modes. Don't auto-hide.
" Each buffer has its own, independent cursor movement.

set guicursor=a:block-blinkon0
set concealcursor= conceallevel=0
set nocursorbind


"""" Appearance: window and minibuffer

" Don't fill my screen with tildes after EOF. Use spaces instead.

set fillchars+=eob:\

" Don't hide the ends of lines that are longer than the window is wide.
" Show a backslash at the window edge, followed by the rest of the line.
" Same thing in the minibuffer.

set wrap nobreakindent showbreak=\
set fillchars+=trunc:\\

" If you're typing a long command, you can open a separate window
" for it. The cedit keybinding triggers this action.

set cedit=<M-e>

" Fix a quirk where Neovim doesn't size the minibuffer properly.

set cmdheight=1

"""" Parentheses

" When I type bracket-type chars (for instance, ')'), highlight the
" opening/closing char that matches. But unhighlight it when I type
" the next char.

set showmatch
call s:SetCpoptionsFlag('m', v:false)

"""" Current working directory

" Don't auto-change the cwd depending on the file I'm editing.

set noautochdir
if exists('+autoshelldir')
	set noautoshelldir
endif

" But make explicit :cd behave like shell cd.

set cdhome
set cdpath=,,

"""" Completion: off by default

set nocomplete

"""" Encoding

" Assume (and write) UTF-8 everywhere. Display glyphs with a width of one
" character wherever allowed. Don't write a Windows-style BOM.

set encoding=utf-8
set fileencoding=utf-8
set ambiwidth=single
set nobomb

" In UTF-8 files, use vim's own idea of uppercase/lowercase,
" not the system locale's idea. Also, don't special-case Turkish ASCII.

set casemap=internal,keepascii

"""" Killing and yanking: system clipboard

" By default, copy/paste to/from the system clipboard.
" You can override this by specifying a different vim "register" to use.

set clipboard=unnamed

"""" Cursor: Emacs-esque keybindings for moving the cursor.
"""" NB: these use vim behavior regarding the nuances of whitespace-
"""" handling and word boundaries.

" Use shift + arrows chords (not C-n and C-p) to move
" among quickfix suggestions.
nnoremap <S-Left> :cprev<CR>
nnoremap <S-Right> :cnext<CR>
nnoremap <C-n> j
nnoremap <C-p> k

nnoremap <C-f> l
nnoremap <C-b> h
nnoremap <C-a> ^
nnoremap <C-e> $
nnoremap <M-f> w
nnoremap <M-b> b
nnoremap <C-v> <PageDown>
nnoremap <M-v> <PageUp>
nnoremap <C-down> }
nnoremap <C-up> {

inoremap <C-n> <Down>
inoremap <C-p> <Up>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <M-f> <Esc>wi
inoremap <M-b> <Esc>bi
inoremap <C-v> <PageDown>
inoremap <M-v> <PageUp>

vnoremap <C-n> j
vnoremap <C-p> k
vnoremap <C-f> l
vnoremap <C-b> h
vnoremap <C-a> ^
vnoremap <C-e> $
vnoremap <M-f> w
vnoremap <M-b> b
vnoremap <C-v> <PageDown>
vnoremap <M-v> <PageUp>
nnoremap <C-t> xp
inoremap <C-t> <Esc>xp i

"""" Emacs keybindings for killing and yanking text

function! EmacsishKillLine()
  if col('.') == col('$')
    return "\<Del>"
  else
    return "\<Esc>d$a"
  endif
endfunction

nnoremap <C-k> d$
inoremap <expr> <C-k> EmacsishKillLine()
vnoremap <C-k> d$

nnoremap <C-y> p
inoremap <C-y> <C-r>"
vnoremap <C-y> p

"""" Emacs keybindings for selecting text

nnoremap <C-@> v
inoremap <C-@> <Esc>vi
vnoremap <C-@> <Esc>

"""" isearch

"" isearch-esque behavior

set incsearch ignorecase smartcase
set hlsearch
set wrapscan
set shortmess-=s
set cpoption-=o

"" isearch keybindings

nnoremap <C-s> /
nnoremap <C-r> ?
inoremap <C-s> <Esc>/
inoremap <C-r> <Esc>?
vnoremap <C-s> <Esc>/
vnoremap <C-r> <Esc>?

cnoremap <C-g> <C-c>
cnoremap <C-w> <C-r><C-w>

"" Reproduce something akin to Emacs query-replace

" query-replace behavior

nnoremap <M-%> :%s///gc<Left><Left><Left>
vnoremap <M-%> :s///gc<Left><Left><Left>

" query-replace-regexp

nnoremap <C-M-%> :%s///gc<Left><Left><Left>
vnoremap <C-M-%> :s///gc<Left><Left><Left>

"""" Buffers and windows

set hidden

" Make vertical splits go to the right of the current window.

set splitright

" Make horizontal splits go below the current window.

set splitbelow

nnoremap <C-x>0 :close<CR>
nnoremap <C-x>1 :only<CR>
nnoremap <C-x>2 :split<CR>
nnoremap <C-x>3 :vsplit<CR>
nnoremap <C-x>o <C-w>w

inoremap <C-x>0 <Esc>:close<CR>i
inoremap <C-x>1 <Esc>:only<CR>i
inoremap <C-x>2 <Esc>:split<CR>i
inoremap <C-x>3 <Esc>:vsplit<CR>i
inoremap <C-x>o <Esc><C-w>w i

"""" Emacs functions for loading/saving/closing files and buffers

nnoremap <C-x><C-s> :w<CR>
nnoremap <C-x><C-f> :edit 
nnoremap <C-x>k :bdelete<CR>
nnoremap <C-x><C-w> :write 
nnoremap <C-x>b :buffer 

inoremap <C-x><C-s> <Esc>:w<CR>i
inoremap <C-x><C-f> <Esc>:edit 
inoremap <C-x>k <Esc>:bdelete<CR>i
inoremap <C-x><C-w> <Esc>:write 
inoremap <C-x>b <Esc>:buffer 

"""" comment-dwim (an intelligent function for commenting the right things
"""" based on context)

" comment-dwim behavior

function! EmacsishCommentRegion()
  let l:save = winsaveview()

  if mode() ==# 'v' || mode() ==# 'V'
    normal! gv
  endif

  let l:start = line("'<")
  let l:end   = line("'>")

  let l:cs = substitute(&commentstring, "%s", "", "")
  let l:pattern = '^\s*' . escape(l:cs, '\')

  let l:all = 1
  for lnum in range(l:start, l:end)
    if getline(lnum) !~ l:pattern
      let l:all = 0
      break
    endif
  endfor

  if l:all
    execute l:start . "," . l:end . "s/^\s*" . escape(l:cs, '\') . "\s\?//"
  else
    execute l:start . "," . l:end . "s/^/" . l:cs . " /"
  endif

  call winrestview(l:save)
endfunction

" comment-dwim keybindings

nnoremap <M-;> :call EmacsishCommentRegion()<CR>

"""" Emacs-esque undo

nnoremap <C-/> u
nnoremap <C-_> u
inoremap <C-/> <Esc>u i
inoremap <C-_> <Esc>u i
vnoremap <C-/> <Esc>u
vnoremap <C-_> <Esc>u

"""" Where it makes sense, add the same emacs keybindings to command mode.

cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>
cnoremap <C-d> <Del>
cnoremap <C-h> <BS>
cnoremap <C-w> <C-w>
cnoremap <M-d> <S-Right><C-w>
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
cnoremap <C-y> <C-r>"
cnoremap <C-g> <C-c>

"""" Emacs-style rectangular text selection, killing, and yanking

" Kill rectangle (Emacs C-x r k)

nnoremap <C-x>rk :call EmacsishKillRectangle()<CR>
vnoremap <C-x>rk :call EmacsishKillRectangle()<CR>

function! EmacsishKillRectangle()
  let g:emacsish_last_rectangle = getline("'<", "'>")
  execute "'<,'>normal! d"
endfunction

" Yank rectangle (Emacs C-x r y)

nnoremap <C-x>ry :call EmacsishYankRectangle()<CR>
vnoremap <C-x>ry :call EmacsishYankRectangle()<CR>

function! EmacsishYankRectangle()
  if !exists("g:emacsish_last_rectangle")
    echo "No rectangle to yank"
    return
  endif
  let l:start = line('.')
  let l:col = col('.')
  for lnum in range(0, len(g:emacsish_last_rectangle)-1)
    call setline(l:start + lnum, getline(l:start + lnum)[:l:col-2] . g:emacsish_last_rectangle[lnum] . getline(l:start + lnum)[l:col-1:])
  endfor
endfunction

" Emacs string-rectangle (C-x r t)

nnoremap <C-x>rt :call EmacsishStringRectangle()<CR>
vnoremap <C-x>rt :call EmacsishStringRectangle()<CR>

function! EmacsishStringRectangle()
  let l:str = input("String rectangle: ")
  execute "'<,'>normal! c" . l:str
endfunction

" Emacs open-rectangle (C-x r o)

nnoremap <C-x>ro :call EmacsishOpenRectangle()<CR>
vnoremap <C-x>ro :call EmacsishOpenRectangle()<CR>

function! EmacsishOpenRectangle()
  execute "'<,'>normal! I "
endfunction

" Emacs clear-rectangle (C-x r c)

nnoremap <C-x>rc :call EmacsishClearRectangle()<CR>
vnoremap <C-x>rc :call EmacsishClearRectangle()<CR>

function! EmacsishClearRectangle()
  execute "'<,'>normal! r "
endfunction

" Miscellaneous behavior

set cpoptions=aAbBcCdDeEfFiIolmnPqstuWXyZ*
