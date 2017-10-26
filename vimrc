let mapleader = ','

""" Basics
set nocompatible
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
" buffer switch
nnoremap gb :ls<CR>:b<Space>

""" Python 3
" augroup python3
"     au! BufEnter *.py setlocal omnifunc=python3complete#Complete
" augroup END

""" Plugins
if has("win32")
    call plug#begin('~/vimfiles/plugged')
else
    call plug#begin('~/.vim/plugged')
endif

" basics
Plug 'tpope/vim-sensible'
Plug 'sheerun/vim-polyglot'

" general purpose
Plug 'sirver/ultisnips'
Plug 'lifepillar/vim-mucomplete'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'

" extra lang syntax
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'maedoc/stan.vim'

" make tables
Plug 'godlygeek/tabular'

" jedi-vim python plugin
Plug 'davidhalter/jedi-vim'

" eye candy
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'

call plug#end()

" Helping code to detect system
if !exists("g:os")
    if has("win64") || has("win32") || has("win16")
	    let g:os="Windows"
    else
	    let g:os=system("uname")
    endif
endif

""" Aesthetics
" Numbers and relative numbers depending on mode
set number relativenumber
augroup numbertoggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END
set cursorline
" if lightline installed:
set noshowmode
" set termguicolors
set t_Co=256 " Is this still necessary?
if has('gui_running')
    set guioptions-=T
    set guioptions-=r
    set guioptions-=R
    " linux subsystem on Windows beeps after error, this should disable it
    set belloff=all
    " let s:uname=system("uname")
    if g:os == "Darwin\n"
    	set guifont=Hack:h15
        colorscheme base16-gruvbox-dark-hard
    elseif g:os == "Linux\n"
	    set guifont=Hack\ 11
        colorscheme Benokai
    elseif g:os == "Windows"
	    set guifont=Hack:h12
        colorscheme Benokai
    endif
    set guicursor+=a:blinkon0 " turn off blinking cursor
    set lines=50
    set columns=90
else
    if g:os == "Darwin\n"
        colorscheme heroku
    endif
endif

""" Text wrapping: wrap when the textwidth limit is reached
" I will put this here because I am not sure a plugin is messing with this
set textwidth=80    " Use gq to reformat (or gqip, gqq)
set colorcolumn=+1  " Relative to the textwidth variable instead of absolute
set nowrap

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
" Pretty text formatting using conceal
let g:pandoc#syntax#conceal#use = 1
let g:pandoc#syntax#codeblocks#embeds#langs = ['python', 'vim', 'make',
            \  'bash=sh', 'html', 'css', 'scss', 'javascript']
" somehow textwidth was reset at 0 when pandoc was loaded
" maybe because the formatting mode defaultes to soft
let g:pandoc#formatting#textwidth = 80
let g:pandoc#formatting#mode = "h"

""" Vimgutter plugin
" shorter update times (default 4s)
set updatetime=1000
