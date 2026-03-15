"""" common.vim: vimrc settings that are shared among vimplementations:
"" 
""     - iOS iVim.app
""     - TUI vim
""     - MacVim
""     - *Not* tested on Neovim
"" 
"""" Many parts cribbed or outright pasted from:
" https://codeberg.org/EvanHahn/dotfiles/commit/91685f0a629b584eb99ba715c441b918b9565ff0/home/vim/.config/nvim/init.vim

" Don't emulate old-school vi.

if has('vim_starting')
    " Set once, at vim start time (resetting after might mess up plugins).
    set nocompatible
endif

" Convenience function for setting vim's single-letter 'cpoptions',
" which control the nitty-gritty details of compatibility with vi.
" We'll call this function later in the file.

function! s:SetCpoptionsFlag(flag, enabled) abort
	if a:enabled
		try
			execute 'set cpoptions+=' . a:flag
		catch /^Vim\%((\a\+)\)\=:E539:/
		endtry
	else
		execute 'set cpoptions-=' . a:flag
	endif
endfunction

"""" Indentation

" Indent using spaces, automatically on RET and manual TAB.

set autoindent
set expandtab
set nocopyindent
set preserveindent " when combined with expandtab, converts tabs to spaces
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Always highlight tabs; I want to see and eliminate them.

highlight ExtraTab guibg=Red guifg=White

" When I activate "list mode", use special glyphs to show certain other undesired
" whitespace et cetera. See `:help listchars` for details.

set list
"" ?? set listchars=conceal:?,extends:\\,nbsp:~,tab:T,trail:~

" - Use a single backspace keypress to delete one level of indentation.
" - Allow backspacing all the way up into the preceding line.
" - Don't stop backspacing at to the char where you entered insert mode.

set backspace=indent,eol,nostop

" Programming-language modes should set their own comment syntax.
" Don't try to guess a default comment char. Ditto with line-ending characters
" that trigger auto-indentation on the following line (for instance, '{').
" Likewise, don't guess the keyword that corresponds to #define in whatever language
" we're using.

set comments=
set commentstring=
set define=

filetype plugin indent on
set nocindent
set nosmartindent

"""" Appearance: Font

" Use my favorite monospace font.

set guifont=Menlo:h16

" Don't use bold or italics. I prefer the uniformity of regular type,
" with highlighting depending on color only.

set guioptions-=b
set guioptions-=i

" Create keybindings to increase/decrease font size.

let mapleader = ","
nnoremap <leader>. :ifont -<cr>
nnoremap <leader>; :ifont +<cr>

"""" Appearance: Visual Bell

" Disable most bells, and make the remainder visual rather than audible.

set visualbell
set noerrorbells
set belloff=all

"""" Appearance: Cursor

" The following settings affect only the GUI version of vim.
" The terminal version just uses whatever cursor the terminal uses.
" 
" - Always solid, never blinking.
" - Block cursor in all modes (except insert mode: vertical-bar cursor).

set guicursor=a:block-blinkon0
" set guicursor+=i:ver-blinkon0

"""" Appearance: Window Title

" Display the filename only. If there's no filename, display nothing.

set title
set titlestring=filename
set titleold=

" Don't hide the cursor or display ligatures. Just show things as they are.

set concealcursor=
set conceallevel=0

" Don't scroll more than one buffer when I move the cursor;
" just scroll the current buffer like a normal text editor.

set nocursorbind

"""" Appearance: Window Behavior

" Don't cleverly autohide any open buffers.

set bufhidden=

" Make vertical splits go to the right of the current window, as in Emacs.

set splitright

" Make horizontal splits go below the current window, as in Emacs.

set splitbelow

" Don't fill my screen with tildes after EOF. Use spaces instead.

set fillchars+=eob:\ ,trunc:\\


"""" Appearance: Long Lines

" For long lines in buffers, wrap and display a prepended Unicode ellipsis.
" Use the same level of indentation for the continuation as for the orig line.

set linebreak " wrap at word boundaries
if exists('+breakindent')
    set nowrap
	set breakindent
	set breakindentopt=min:40,sbr
    set showbreak=…
else
    " But if that sophisticated wrapping isn't available,
    " at least do basic wrapping (as opposed to hiding the ends of long lines
    " until I move the cursor far enough to the right).
    set wrap
endif

"""" Programming-Language Syntax

" Load my custom color theme if it's installed; otherwise load the default theme.

set background=dark
set hidden

try
    colorscheme tritium
catch /^Vim\%((\a\+)\)\=:E185:/
    colorscheme default
endtry

" Alternative: load the `dim` colorscheme if it's installed.
" This theme just uses your terminal's ANSI colors.

" try
"     colorscheme vim-dim
" catch /^Vim\%((\a\+)\)\=:E185:/
"     colorscheme default
" endtry

" Automatically perform syntax highlighting on known filetypes.
" filetypes are defined in `$VIMRUNTIME/filetype.vim` (vim)
" or `$VIMRUNTIME/filetype.lua`(Neovim).

if has('syntax')
    syntax enable

    " Don't highlight the line or column where the cursor is.
    set nocursorcolumn
    set nocursorline
    " cursorlineopt=line,number
endif

" For long lines, stop syntax highlighting after this many chars.

set synmaxcol=240

" When I type bracket-type chars (for instance, ')'), highlight the
" opening/closing char that matches. But unhighlight it when I type
" the next char.

set showmatch
call s:SetCpoptionsFlag('m', v:false)

"""" Current Working Directory

" Don't auto-change vim's cwd to that of the currently open file,
" as that can mess up plugins.

set noautochdir

" But, if we're using vim's built-in terminal, *do* change that shell's cwd.

if exists('+autoshelldir')
	set autoshelldir
endif

" When I run `:cd` with no arguments, change to my home dir,
" as in Unix shells.

set cdhome

" Explicit `:cd dir` should look for `dir` in the current directory,
" as in Unix shells.

set cdpath=,,

"""" Clipboard

" By default, copy/paste to/from the system clipboard.
" You can override this by specifying a different vim "register" to use.

set clipboard=unnamed

"""" Completion

" Places where completion should look for tokens:
"   . - contents of current buffer
"   b - other buffers in this window
"   w - other buffers in other windows
"   f - buffer names (Neovim only; does something different in vim).

set complete=.,w,b
if has('neovim')
	set complete+=f
endif

" Uncomment if you want fuzzy completion (like Emacs ido but more so).
" 
" if has('patch-9.1.0463')
" 	set completeopt+=fuzzy
" endif

" Don't automatically show completions as I type; it's distracting.
" Show them only when I press the completion key.

set noautocomplete

" Set the format of autocomplete-menu items:
"   abbr - the completion candidate
"   kind - something like "Method" or "Field"
"   menu - extra verbiage, which varies. I like that order.

if exists('+completeitemalign')
	set completeitemalign=abbr,menu,kind
endif

" Completion UI behavior:
"
" longest - at the cursor, show only the longest common prefix (as in Emacs)
" noinsert - 

"" ?? set completeopt=longest,menu,preview,popup,insert,select

" Set the completion key to M-Tab , not C-n, as I'm used to C-n having
" a different meaning in Emacs.

"" ?? unmap <C-n>
"" ?? unmap <C-p>
inoremap <M-Tab> <C-n>
nnoremap <M-Tab> <C-n>

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

" Make backspace delete Unicode combining characters piece by piece;
" don't delete the whole character at once.

set delcombine

"""" Large Files

" For really huge files, turn off slow features like autosave, soft line wrap,
" and even syntax highlighting.

function! s:HandleLargeFiles() abort
	if getfsize(expand('%')) > 100000000 " 100 MB
        setlocal nowrap nolinebreak nofoldenable foldmethod=manual
        setlocal noswapfile nobackup nowritebackup undolevels=1
        setlocal patchmode=
        if has('syntax')
            syntax disable
        endif
    endif
endfunction

autocmd BufReadPre * call s:HandleLargeFiles()

"""" Searching and Regexes

set history=1024

" With `/` (search) and realted commands,
" when I type lowercase letters, match case-insensitively,
" BUT the moment I type an uppercase letter,
" treat the whole search term case-sensitively.
"
" This behavior is known as smartcase, and it corresponds to
" the default behavior of Emacs isearch.

set ignorecase smartcase

" Also as in Emacs isearch: search as I type; don't wait for me to hit RET.

set incsearch

"""" Navigation

" Disable support for writing right-to-left languages,
" so that I don't accidentally turn it on.

set noallowrevins
set noarabic
set noarabicshape

"""" echo area ("minibuffer", in Emacs terminology)

" If you're typing a long command, you can open a separate window
" for it. The cedit keybinding triggers this action. cmdwinheight
" specify's the windows height in lines.
"" ?? set cedit=<M-e>
set cmdwinheight=5

" But, when I'm not using a separate window, just use a one-line echo area
" (overriding neovim's buggy default of 0).

set cmdheight=1

"""" Settings specific to GUI vim (as opposed to TUI vim).

" When I open a OS-native file browser dialog,
" start at the current working directory.
" Ignore errors from vim variants that don't support this (neovim?)

if has('gui')
	try
		set browsedir=current
	catch /^Vim\%((\a\+)\)\=:E519:/
	endtry
endif

"""" Saving, swapfiles, and backups.
"""" The semantics are way too nuanced, as you are about to learn...

" Prevent accidental quit-without-saving

set confirm

" Don't use swap files, but do use undo files.

set noswapfile
set nobackup
set nowritebackup
"" set undodir=~/ivim/backups
set undofile

" Autosave from time to time.

set autowrite
set autowriteall

" Automatically reload the file if it's changed outside of Vim.

set autoread

"" Do make a backup file before writing (for atomicity),
"" but don't leave the backup file hanging around after a successful write.

" When writing to an existing file, I want to make a backup in case of
" catastrophe; that's why I enable `writebackup`. However, I don't want this
" backup to stick around after the write is finished, because (1) I don't want
" to clutter up my disk (2) I don't want to accidentally save something
" sensitive somewhere. Therefore, I disable the `backup` option.
"
" I think Vim's docs for backup (`:help backup`) are weak, and [this Stack
" Exchange answer][0] explains things much more clearly.
"
" [0]: https://vi.stackexchange.com/a/16846

set nobackup

" When writing to an existing file, a backup is created if `backup` or
" `writebackup` are enabled. Then one of two things happens:
"
" 1. The original is copied, then overwritten
" 2. The original is renamed and a new file is written in its place
"
" Most of the time, the difference here doesn't matter. But reading
" `:help 'backupcopy'` reveals many subtleties:
"
" - Renaming is faster than copying.
" - File attributes, like the owner and group, may or may not be changed.
" - Symlinks and hardlinks can have subtle issues.
" - Renaming could cause issues with tools that are looking at the file
"   descriptor, like inotify. After all, the updated file will have a new file
"   descriptor.
"
" Most of the time, these subtleties are irrelevant, and I have not noticed
" any issues, so I choose the "auto" option.
set backupcopy=auto

" In Neovim, store backup files in the state dir. In vanilla Vim, use a
" backup directory in `~/.vim`.

if has('nvim')
	let &backupdir = stdpath('state') . '/backup/'
else
	let &backupdir = expand('$HOME/.vimruntime/backup/')
endif

" Use a very explicit backup filename.

set backupext=.vim-backup

" In the rare case that I'm using a Vim distribution that has the
" `balloon_eval` feature enabled, disable it because I never use it. Neovim
" removes this feature and I think it's only in GUI versions of vanilla Vim.
" Because this is disabled and I don't know much about this feature, I ignore
" related options `balloondelay` and `balloonexpr`.

if has('balloon_eval')
	set noballooneval
endif
if has('balloon_eval_term')
	set noballoonevalterm
endif

" Disable built-in plugins that I never use.

let g:loaded_2html_plugin = 1 " to convert your code to HTML-with-syntax-colors
let g:loaded_logipat = 1  " boolean expressions, for people who dislike regexes
let g:loaded_getscriptPlugin = 1 " to sync vimscript from vim.org's repo
let g:loaded_rrhelper = 1 " for sending remote commands to a vim session

"""" cpoptions: selectively enable/disable vi-compatible behavior

" 'cpoptions' are one-letter options that control various edge-case behaviors,
" all related to vi compatilibility. Sometimes I prefer the vi way,
" and sometimes I prefer the default vim way.
"
" Your system-wide vim config might set some of these, so let's start by
" resetting cpoptions to the default value that you get if you do 'vim -u NONE':

set cpoptions=aAbBcCdDeEfFgHiIjJkoKlLmMnoOpPqrRsStuvwWxXyZz$!%*-+<>;

" Okay, now we can override those defaults that I don't like. We'll call this
" SetCpoptions convenience function for that purpose:

" Do not interpret a backslash in regex character classes, like [\t], literally.
" Interpret it as an escape. Do ':h cpo-l' for an example of why the literal approach
" is bad.

call s:SetCpoptionsFlag('b', v:false)
call s:SetCpoptionsFlag('l', v:false)

" When searching repeatedly for a regex match, start each successive search
" at the end of last match; don't allow overlapping matches.
" That is, make it so that
"   abababababab
" only gets three matches when repeating
"   /abab
" and not five matches!
" 
" In fact, this cpoption is actually set on/true by default, but it's
" weird enough and important enough to document by setting it explicitly:

call s:SetCpoptionsFlag('c', v:true)

" Don't move the cursor to the first line when opening a file. To do so would
" defeat Vim's feature where it remembers the cursor position from last time
" we edited the file.

call s:SetCpoptionsFlag('g', v:false)

" When running the 'I' command on a whitespace-only line, position the cursor
" after the last character, not before the last character!

call s:SetCpoptionsFlag('H', v:false)

" Suppose you indent, then decide to hit RET without typing any text.
" Do you want the indentation whitespace to remain?
" If not, change this to false. The whole line will be replaced with a single newline.

" I set this to true because I actually like having this whitespace, which follows
" the Emacs default... but many style guides prohibit it.

call s:SetCpoptionsFlag('I', v:true)

" Never auto-insert double space after sentences (I use a single space).

set nojoinspaces
call s:SetCpoptionsFlag('j', v:false)
call s:SetCpoptionsFlag('J', v:false)

" I don't completely understand the `k` flag, but it's exclusive to vanilla
" Vim (removed from Neovim in `3baba1e7bc6698e6bc9f1d37fce88b30d6274bc9`) and
" I don't think I want it enabled.

call s:SetCpoptionsFlag('k', v:false)

" Wait for key codes to complete when halfway through a mapping; don't time out.
" In Neovim, this option seems to do be a no-op, whether set or not.

call s:SetCpoptionsFlag('K', v:false)

" Don't count a tab as two characters when `list` is enabled. Count it as one.

call s:SetCpoptionsFlag('L', v:false)

" Don't count \( as matching ) or vice versa.

call s:SetCpoptionsFlag('M', v:false)

" When you do a search, you can specify an offset (see `:help search-offset`).
" Where `/foo/` will put your cursor right on top of the next occurrence of
" `foo`, `/foo/+5` will put your cursor five lines down from the next
" occurrence. (I'm not sure when this would be useful but I'm sure there's a
" good time.) If I ever do this, I want to press `n` and have it remember that
" line offset. Therefore, I do not set the `o` flag.

call s:SetCpoptionsFlag('o', v:false)

" Give an error message, and don't autosave, if the following sequence takes place:
" (1) I run `vim file.txt` when `file.txt` doesn't exist
" (2) `file.txt` is created elsewhere
" (3) I try to write that file.

call s:SetCpoptionsFlag('O', v:false)

" Don't use vi-style Lisp indentation. Use vim's somewhat better algorithm.

call s:SetCpoptionsFlag('p', v:false)

" When you join more than two lines, where should the cursor go? With this
" flag true, it goes between the first and second lines. With this
" flag false, it goes between the penultimate and last lines.

call s:SetCpoptionsFlag('q', v:true)

" IF you perform an action with a search motion (like `d/foo`) and then change
"    the search (like `/bar`), and then use `.` to repeat the action,
" 
" THEN use the original search, not the new one. Thanks to this StackExchange
" answer: https://vi.stackexchange.com/a/46864/56767

call s:SetCpoptionsFlag('r', v:false)

" Don't remove marks from filtered lines. See `:help filter`.

call s:SetCpoptionsFlag('R', v:false)

" Let buffer options be per-buffer; don't auto-copy them to other buffers
" when I visit them.

call s:SetCpoptionsFlag('S', v:false)

" When I press `uu`, undo two changes; don't undo and then undo the undoing.
" See `:help undo-two-ways`.

call s:SetCpoptionsFlag('u', v:false)

" When I backspace over characters, erase them. Don't leave them visible like vi.
" Same for other changes ('$').

call s:SetCpoptionsFlag('v', v:false)
call s:SetCpoptionsFlag('$', v:false)

" When I do `cw` on a blank char, delete all the whitespace, not just the first char.
" I think this setting is ignored in Neovim.

call s:SetCpoptionsFlag('w', v:false)

" In command mode, if I press `<Esc>`, abandon the command; don't execute it!

call s:SetCpoptionsFlag('x', v:false)

" Vim's documentation for its exclusive `z` flag isn't super clear,
" but I don't think I want to enable it. See `:help cpo-z`.

call s:SetCpoptionsFlag('z', v:false)

" When redoing a filter, reuse the last filter command, not the last external
" command (which might be different).

call s:SetCpoptionsFlag('!', v:false)

" Use more advanced behavior for the `%` command. But consider using
" the `matchit` plugin, as Neovim does, to avoid the weird semantics of %

call s:SetCpoptionsFlag('%', v:false)

" A vertical movement shouldn't fail when out of range.
" For example, `99999999k` should take you to the first line, not fail.
" I think this flag doesn't exist in Neovim.

call s:SetCpoptionsFlag('-', v:false)

" Don't mark buffer A as saved when I save buffer B! Continue marking A as `modified`.
" 
" (NB: in vim, "mark buffer A as saved" really just means "cease marking buffer A as
" `modified`).

call s:SetCpoptionsFlag('+', v:false)

" Recognize `<` and `>` in key mappings, so that things like `:map X <Tab>`
" works. I think this option doesn't exist in Neovim, and this behavior
" is always implicitly on?

call s:SetCpoptionsFlag('<', v:false)

" Don't put a line break before text appended to a register.

call s:SetCpoptionsFlag('>', v:false)

" Make `,` and `;` keep going after something is found, not halt.

call s:SetCpoptionsFlag(';', v:false)

" Make `cw` clear the whitespace at the end of a word, as `dw` does.
call s:SetCpoptionsFlag('_', v:true)

"""" Keybindings

map ; :
let mapleader = ","

"" In vim as in Emacs, Esc-<key> is interpreted as M-<key>... but only
"" if the interval between Esc and <key> is short. Tune that interval.
"" It's especially important to get this right in vim, since Esc on its own
"" is used to exit insert mode and enter normal mode.

" After this many milliseconds, treat it as Esc-on-its-own.

set timeoutlen=500

" dIf I start typing a key sequence and then wait more than 50 milliseconds, I
" want that to time out. This is a smaller number because many key sequences
" are me logically pressing a key, but the terminal receiving multiple keys.

set ttimeout
set ttimeoutlen=50

" Make Y yank from cursor to EOL, consistent with other uppercase commands; see
" https://vi.stackexchange.com/questions/6061/why-is-y-a-synonym-for-yy-instead-of-y
" This is the default in Neovim, but setting it explicitly doesn't hurt.

nnoremap Y y$

" I'm used to C-w having a different meaning in Emacs,
" so make it a no-op and bind its original command to M-w.

"" ?? iunmap <C-w>
"" ?? inoremap <M-w> <C-w>

" Likewise, remap C-* bindings that are used for navigation in Emacs.

"" ?? iunmap <C-n>
"" ?? iunmap <C-p>
"" ?? inoremap <M-n> <C-n>
"" ?? inoremap <M-p> <C-p>

" Use shift + arrows chords to move among quickfix suggestions.

nnoremap <S-Left> :cprev<CR>
nnoremap <S-Right> :cnext<CR>

" Use C-. to open the file at point, as I do in Emacs.

nnoremap <C-.> gf
vnoremap <C-.> gf
inoremap <C-.> <Esc>gfi

" Unmap keybindings that are too easy to type by accident.
" There are other bindings for these, anyway.

nnoremap ZQ <nop> " same as :wq
nnoremap ZZ <nop> " same as :q!
nnoremap Q <nop> " starts ex mode, which I never want.
