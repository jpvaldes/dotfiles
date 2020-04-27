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
" Plug 'itchyny/lightline.vim'
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'
Plug 'joshdick/onedark.vim'
Plug 'patstockwell/vim-monokai-tasty'

" deoplete
if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    " back to deoplete-jedi from ALE + pyls until pyls fixes #676
    Plug 'deoplete-plugins/deoplete-jedi'
endif

" snippets
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

" Ale
Plug 'dense-analysis/ale'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Vim-slime
" You can type text in a file, send it to a live REPL, and avoid having to
" reload all your code every time you make a change.
Plug 'jpalardy/vim-slime'
let g:slime_target = "tmux"
let g:slime_python_ipython = 1

call plug#end()

""" Basics
let mapleader = ','
" shorter update times (default 4s)
set updatetime=100
set backspace=indent,eol,start
syntax on
filetype plugin indent on
set ruler
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
" termguicolors
set termguicolors

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
inoremap <expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
" call FZF with F9
noremap <silent><F9> :FZF<CR>
" clear the search highlights
nnoremap <C-l> :nohlsearch<CR>
" Neosnippet key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-j>     <Plug>(neosnippet_expand_or_jump)
smap <C-j>     <Plug>(neosnippet_expand_or_jump)
xmap <C-j>     <Plug>(neosnippet_expand_target)
" ALE
nmap <silent> <Leader>n <Plug>(ale_next_wrap)
nmap <silent> <Leader>N <Plug>(ale_previous_wrap)

let g:deoplete#enable_at_startup = 1

let g:neosnippet#snippets_directory='~/dotfiles/snips'

" colorscheme
set cursorline
let g:gruvbox_italic = 1
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_sign_column = 'bg0'
let g:gruvbox_color_column = 'bg0'
colorscheme gruvbox

let g:lightline = {
  \ 'colorscheme': 'one'
  \ }

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

    " return l:counts.total == 0 ? 'OK' : printf(
    " \   '%dW %dE',
    " \   all_non_errors,
    " \   all_errors
    return printf(
    \   '[%dW %dE]',
    \   all_non_errors,
    \   all_errors
    \)
endfunction

set statusline =
set statusline +=\ %f
set statusline +=\ %y%m
set statusline +=\[%{mode()}\]
set statusline +=%h%r%w
" Right align
set statusline +=%=
set statusline +=\%{LinterStatus()}
set statusline +=%{FugitiveStatusline()}
" Row, column and total rows
set statusline +=\ %v\:%l\/%L
set statusline +=\ [%n]

" new floating windows
if exists("*nvim_open_win")
    " transparency for floating windows (0 is none)
    set winblend=10

    let g:fzf_layout = { 'window': 'call FloatingFZF()' }
endif

function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  " % of total height
  let height = float2nr(&lines * 0.6)
  " % of total width
  let width = float2nr(&columns * 0.7)
  let horizontal = float2nr((&columns - width) / 2)
  let vertical = 10

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction
