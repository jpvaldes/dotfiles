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
Plug 'sheerun/vim-polyglot'

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

" Markdown support
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
let g:vim_markdown_folding_level = 3

" Jedi
Plug 'davidhalter/jedi-vim'

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

" colorscheme
set cursorline
let g:srcery_italic = 1
let g:srcery_bold = 1
colorscheme srcery

if has("gui_running")
    set guifont=Iosevka\ Term\ Medium\ 17,Hack\ 16
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

set laststatus=2
set noruler
set noshowmode
set statusline =%y
set statusline +=\[%{mode()}\]
set statusline +=%m
set statusline +=\ %.20F
set statusline +=%h%r%w
" Right align
set statusline +=%=
set statusline +=\%{LinterStatus()}
set statusline +=%{FugitiveStatusline()}
" Row, column and total rows
set statusline +=\ %3v\:%-3.4l\/%-4L
set statusline +=\ [%n]
