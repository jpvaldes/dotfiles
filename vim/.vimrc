set nocompatible

""" Plugins
" Autoload
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

if has("win32")
    call plug#begin('~/vimfiles/plugged')
else
    call plug#begin('~/.vim/plugged')
endif

" basics
" Plug 'sheerun/vim-polyglot'

" general purpose
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" git plugins
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" see contents of registers in insert mode
Plug 'junegunn/vim-peekaboo'

" colorschemes
Plug 'chriskempson/base16-vim'
Plug 'joshdick/onedark.vim'
Plug 'patstockwell/vim-monokai-tasty'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'vimoxide/vim-cinnabar'
Plug 'srcery-colors/srcery-vim'

" snippets
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

" Ale
Plug 'dense-analysis/ale'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Tabular support
Plug 'godlygeek/tabular'

" Jedi
Plug 'davidhalter/jedi-vim'

" Nim
" This plugin is Python-based
Plug 'zah/nim.vim'

" Autocomplete
Plug 'lifepillar/vim-mucomplete'

" Vim-slime
" You can type text in a file, send it to a live REPL, and avoid having to
" reload all your code every time you make a change.
Plug 'jpalardy/vim-slime'

" Cheat sheet
" Default bind is <leader>?
Plug 'lifepillar/vim-cheat40'

call plug#end()

""" Basics
let mapleader = ','
" shorter update times (default 4s)
set updatetime=100
" eliminate delays after pressing ESC
" separate mapping and keycode timeouts
set timeout ttimeout
" mapping timeout
set timeoutlen=1000
" keycode timeout
set ttimeoutlen=10
set backspace=indent,eol,start
syntax on
filetype plugin indent on
set history=1000
set hidden
" set spell
set spelllang=en_us
" stop certain movements from always going to the first character of a line
set nostartofline
" Whether to treat underscore *_* as word (but not WORD) separator
" set iskeyword-=_
" Swap and backup files under .vim instead of cluttering the working dir
" Step 1: check dir exists and create if needed
if !isdirectory($HOME . "/.vim/backup")
    call mkdir($HOME . "/.vim/backup", "p", 0700)
endif
if !isdirectory($HOME . "/.vim/undo")
    call mkdir($HOME . "/.vim/undo", "p", 0700)
endif
if !isdirectory($HOME . "/.vim/swp")
    call mkdir($HOME . "/.vim/swp", "p", 0700)
endif
" Step 2: set directories
set undodir=~/.vim/undo/
set backupdir=~/.vim/backup/
set directory=~/.vim/swp/

" set termguicolors
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

""" Encoding
set encoding=utf-8
setglobal fileencoding=utf-8
if empty(&termencoding)
  let &termencoding=&encoding
endif

""" Tabs
set autoindent
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4

""" Search
set hlsearch
set incsearch
set showmatch
set ignorecase
set smartcase " don't ignore capital letters

""" keep some lines above or below when scrolling
set scrolloff=3

" well, just to resize
set mouse=a

""" splits below and right, instead of top and left
set splitbelow
set splitright

""" unnamed register
if has('unnamedplus')
    set clipboard=unnamed,unnamedplus
endif

""" live substitution
" options 1) "", 2) nosplit, 3) split
if exists('&inccommand')
    set inccommand=split
endif

""" do not force equal splits
set noequalalways

""" Mappings
" explore in vertical split
nnoremap <Leader>e :Explore! <enter>
" saving and closing buffers
nnoremap <Leader>w :w <enter>
nnoremap <Leader>q :bd <enter>
" access the copy buffer
nnoremap <Leader>x "+
" buffer switch
nnoremap gb :ls<CR>:b<Space>
" Buffers
nnoremap <Leader>b :Buffers<CR>
" Files
nnoremap <Leader>f :Files<CR>
" delete trailing whitespace
nnoremap <Leader>kw :%s/\s\+$//<CR>
" TAB to cycle through completion options
" inoremap <expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
" inoremap <expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
" call FZF with F9
noremap <silent><F9> :FZF<CR>
" clear the search highlights
nnoremap <C-l> :nohlsearch<CR>
" Neosnippet key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-e>     <Plug>(neosnippet_expand_or_jump)
smap <C-e>     <Plug>(neosnippet_expand_or_jump)
xmap <C-e>     <Plug>(neosnippet_expand_target)

let g:neosnippet#snippets_directory='~/dotfiles/snips'

" markdown
" this options do not work in after/ftplugin
let g:markdown_folding = 1
let g:markdown_fenced_languages = ['html', 'css', 'python', 'bash=sh', 'nim']
set foldlevelstart=1

" colorscheme
" Put the cursor over an element and find its name with 
" `:echo synIDattr(synID(line('.'), col('.'), 1), 'name')`
set cursorline
function! Highlight(group, fg, bg, style)
  exec "hi " . a:group
        \ . " ctermfg=" . a:fg["cterm"]
        \ . " ctermbg=" . a:bg["cterm"]
        \ . " cterm=" . a:style["cterm"]
        \ . " guifg=" . a:fg["gui"]
        \ . " guibg=" . a:bg["gui"]
        \ . " gui=" . a:style["gui"]
endfunction
let s:italic = { "cterm": "italic", "gui": "italic" }
let s:bold = { "cterm": "bold", "gui": "bold" }
let s:underline = { "cterm": "underline", "gui": "underline" }
let s:bold_underline = { "cterm": "bold,underline", "gui": "bold,underline" }
let s:none = { "cterm": "NONE", "gui": "NONE" }
let s:orange = { "cterm": 233, "gui": "#FD971F" }
let s:tasty_orange = { "cterm": 208, "gui": "#FF9700" }
let s:light_green = { "cterm": 148, "gui": "#A4E400" }
let s:light_blue = { "cterm": 81, "gui": "#62D8F1" }
let s:magenta = { "cterm": 197, "gui": "#FC1A70" }
let s:purple = { "cterm": 141, "gui": "#af87ff" }
let s:red = { "cterm": 162, "gui": "#ff3176" }
let s:light_red = { "cterm": 219, "gui": "#f48fb1" }
let s:white = { "cterm": 231, "gui": "#ffffff" }
let s:light_grey = { "cterm": 250, "gui": "#bcbcbc" }
let s:grey = { "cterm": 245, "gui": "#8a8a8a" }
let s:dark_grey = { "cterm": 59, "gui": "#5f5f5f" }
let s:darker_grey = { "cterm": 238, "gui": "#444444" }
let s:light_charcoal = { "cterm": 238, "gui": "#2b2b2b" }
let s:charcoal = { "cterm": 235, "gui": "#262626" }
" molokai but modified below with elements from monokai-tasty
colorscheme molokai
" Fix invisible paren
call Highlight("MatchParen", s:orange, s:charcoal, s:italic)
call Highlight("Comment", s:grey, s:none, s:italic)
" Make the red less red
call Highlight("Conditional", s:red, s:none, s:none)
call Highlight("ErrorMsg", s:red, s:none, s:none)
call Highlight("Keyword", s:red, s:none, s:none)
call Highlight("Operator", s:red, s:none, s:none)
call Highlight("Repeat", s:red, s:none, s:none)
call Highlight("SpecialChar", s:red, s:none, s:none)
call Highlight("Statement", s:red, s:none, s:none)
call Highlight("Tag", s:red, s:none, s:none)
" GitGutter
call Highlight("GitGutterAdd", s:light_green, s:none, s:bold)
call Highlight("GitGutterChange", s:light_blue, s:none, s:bold)
call Highlight("GitGutterDelete", s:magenta, s:none, s:bold)
call Highlight("GitGutterChangeDelete", s:orange, s:none, s:bold)
" Python
" hi link pythonTripleQuotes Comment
hi link pythonDoctest Comment
hi link pythonDoctest2 Comment

" call Highlight("mkdHeading", s:magenta, s:none, s:none)
call Highlight("mkdURL", s:light_green, s:none, s:none)
call Highlight("mkdLink", s:light_blue, s:none, s:underline)
call Highlight("mkdCode", s:orange, s:none, s:none)
call Highlight("mkdCodeStart", s:light_green, s:none, s:none)
call Highlight("mkdCodeEnd", s:light_green, s:none, s:none)
call Highlight("mkdDelimiter", s:purple, s:none, s:none)

if has("gui_running")
    set guifont=Hack\ 12
endif

" Ignore certain files and folders when globbing
set wildignore+=*.o,*.obj,*.bin,*.dll,*.exe
set wildignore+=*/.git/*,*/.svn/*,*/__pycache__/*,*/build/**,*.pyc
set wildignore+=*.jpg,*.png,*.jpeg,*.gif,*.bmp,*.tiff
set wildignore+=*.DS_Store
set wildignore+=*.aux,*.bbl,*.blg,*.brf,*.fls,*.fdb_latexmk,*.synctex.gz,*.pdf

" simple statusline
function! LinterStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return printf(
    \   '[%dW %dE]',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

function! GitStatus() abort
    "Do not print `Git[()] from fugitive
    let l:currentbranch = exists("g:loaded_fugitive")?fugitive#statusline():""
    let l:currentbranch = substitute(l:currentbranch, '[Git(', '', '')
    let l:currentbranch = substitute(l:currentbranch, ')]', '', '')
    return l:currentbranch
endfunction

set laststatus=2
set noruler
set noshowmode
set statusline =%y
set statusline +=\[%{mode()}\]
set statusline +=%m
set statusline +=\ %.24F
set statusline +=%h%r%w
" Right align
set statusline +=%=
set statusline +=\%{LinterStatus()}
set statusline +=\ %.24{GitStatus()}
" Row, column and total rows
set statusline +=\ %3v\:%-3.4l\/%-4L
set statusline +=\ [%n]
