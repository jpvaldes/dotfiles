let mapleader = ','

""" Basics
set nocompatible
set backspace=indent,eol,start
syntax on
filetype plugin indent on
set ruler
set history=1000
set hidden
set spell spelllang=en_us
" stop certain movements from always going to the first character of a line
set nostartofline

""" Encoding
if has('multi_byte')
   " Encoding used by the terminal
   if empty(&termencoding)
      let &termencoding=&encoding
   endif
   " encoding used in buffers, files, registers, etc
   set encoding=utf-8
   " encoding used for writing files
   setglobal fileencoding=utf-8
endif

""" Tabs
set autoindent
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4

""" Text wrapping
set textwidth=79
set colorcolumn=80
set nowrap

""" Search
set hlsearch
set incsearch
set showmatch
set ignorecase
set smartcase " don't ignore capital letters
" nnoremap <leader><space> :nohls <enter> " clear highlighted searches

""" NetRW
let g:netrw_liststyle = 1
let g:netrw_sizestyle = "H"
let g:netrw_banner = 0
" explore in vertical split
nnoremap <Leader>e :Explore! <enter>

""" Mappings
nnoremap <Leader>w :w <enter>
nnoremap <Leader>q :bd <enter>
" access the copy buffer
nnoremap <Leader>x "+

""" Python
augroup python3
    au! BufEnter *.py setlocal omnifunc=python3complete#Complete
augroup END

""" Plugins
if has("win32")
    call plug#begin('~/vimfiles/plugged')
else
    call plug#begin('~/.vim/plugged')
endif

" basics
Plug 'tpope/vim-sensible'
Plug 'sheerun/vim-polyglot'
Plug 'flazz/vim-colorschemes'

" general purpose
Plug 'sirver/ultisnips'
Plug 'lifepillar/vim-mucomplete'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'

" extra syntax
Plug 'vim-pandoc/vim-pandoc-syntax'

" make tables
Plug 'godlygeek/tabular'

" jedi-vim python plugin
Plug 'davidhalter/jedi-vim'

call plug#end()

" Aesthetics
set number
set cursorline
colorscheme 
" set termguicolors
set t_Co=256 " Is this still necessary?
if has('gui_running')
    set guioptions-=T
    set guioptions-=r
    set guioptions-=R
    colorscheme monokain
    set guifont=Hack\ 12
    set guicursor+=a:blinkon0 " turn off blinking cursor
    set lines=60
    set columns=90
endif

""" Autocompletion - mucomplete
set completeopt=menuone,noinsert,noselect
inoremap <expr> <c-e> mucomplete#popup_exit("\<c-e>")
inoremap <expr> <c-y> mucomplete#popup_exit("\<c-y>")
inoremap <expr>  <cr> mucomplete#popup_exit("\<cr>")
set shortmess+=c " shut off completion messages
let g:mucomplete#enable_auto_at_startup = 1
" integrate with ultisnips
call add(g:mucomplete#chains['default'], 'ulti')
" ultisnips expand trigger
let g:UltiSnipsExpandTrigger="<C-J>"
let g:UltiSnipsJumpForwardTrigger="<C-J>"
let g:UltiSnipsJumpBackwardTrigger="<C-K>"

""" Pandoc plugin
augroup pandoc_syntax
    au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
    au! BufNewFile,BufFilePre,BufRead *.markdown set filetype=markdown.pandoc
augroup END

let g:pandoc#syntax#conceal#use = 0
let g:pandoc#syntax#codeblocks#embeds#langs = ['python', 'vim', 'make',
            \  'bash=sh', 'html', 'css', 'scss', 'javascript']
