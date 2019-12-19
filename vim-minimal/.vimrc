" Use Vim settings
set nocompatible

" Make backspace behave in a sane manner
set backspace=indent,eol,start

" Syntax highlighting
syntax on

" File type detection and language independent indenting
filetype plugin indent on

" Minimal Python config
autocmd Filetype python set expandtab smarttab ts=4 sw=4

" UTF-8 encoding
set enc=utf-8
set fenc=utf-8
set termencoding=utf-8

" Use indentation of previous line
set autoindent

" Use intelligent indentation
set smartindent

" Insert spaces instead of tabs and configure tabwidth
" Some of this is already setup for Python
set tabstop=4
set shiftwidth=4
set expandtab

" Line numbers
set number

" Highlight matching braces
set showmatch

" Terminal 256 colors
set t_Co=256

" Colorscheme
set background=dark
colors elflord

" Options from vim-sensible
" Delete comment character when joining commented lines
if v:version > 703 || v:version == 703 && has("patch541")
    set formatoptions+=j
endif
set complete-=i
set laststatus=2
" set ruler
set wildmenu
" Load matchit.vim if the user hasn't installed a newer version
if !exists('g:loaded_mathit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif
