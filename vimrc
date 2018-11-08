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
Plug 'tpope/vim-sensible'
Plug 'sheerun/vim-polyglot'

" general purpose
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'

" async syntax checking (older alternative is syntastic)
Plug 'w0rp/ale'

" git plugin
Plug 'tpope/vim-fugitive'

" see contents of registers in insert mode
Plug 'junegunn/vim-peekaboo'

" Display tags/classes in a window
Plug 'majutsushi/tagbar'

" extra lang syntax
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'maedoc/stan.vim'

" distraction free writing
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

" make tables
Plug 'dhruvasagar/vim-table-mode'
Plug 'godlygeek/tabular'

" python plugin
" Plug 'cjrh/vim-conda'
Plug 'davidhalter/jedi-vim'
" Plug 'tell-k/vim-autopep8'

" autocompletion
if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'zchee/deoplete-jedi'
Plug 'ujihisa/neco-look'  " look dictionary completion
Plug 'jpvaldes/deoplete-biblatex'
let g:deoplete#enable_at_startup = 1

" snippets
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

" eye candy
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'
Plug 'joshdick/onedark.vim'
Plug 'rakr/vim-one'
Plug 'romainl/flattened'
Plug 'lifepillar/vim-solarized8'
" Plug 'thaerkh/vim-indentguides' " display indentation guides
" Plug 'maximbaz/lightline-ale' " display ale status in status line

" task management
Plug 'soywod/kronos.vim'

" tmux completion
" yes, this exists
" Plug 'wellle/tmux-complete.vim'

call plug#end()

""" Basics
let mapleader = ','
set backspace=indent,eol,start
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
    set termguicolors
endif
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

""" Encoding
if has('multi_byte')
   " encoding used in buffers, files, registers, etc
   set encoding=utf-8
   " encoding used for writing files
   setglobal fileencoding=utf-8
   " Encoding used by the terminal
   if empty(&termencoding)
      let &termencoding=&encoding
   endif
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

""" keep some lines above or below when scrolling
set scrolloff=3

" well, just to resize
set mouse=a

""" splits below and right, instead of top and left
set splitbelow
set splitright

""" NetRW
let g:netrw_liststyle = 1
let g:netrw_sizestyle = "H"
let g:netrw_banner = 0

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
" next/previous buffers
nnoremap <Leader>b :bnext<CR>
nnoremap <Leader>B :bprev<CR>
" delete trailing whitespace
nnoremap <Leader>kw :%s/\s\+$//<CR>
" nerdtree shortcut
noremap <silent> <F3> :NERDTreeToggle<CR>
" tagbar toggle
nmap <silent> <F4> :TagbarToggle<CR>

" ale plugin
let g:ale_completion_enabled = 0
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '➤'

" deoplete options after plug#end
" call deoplete#custom#option('sources', {
"             \ '_': ['buffer'],
"             \ 'vim': ['vim']
"             \ })
let g:deoplete#auto_completion_start_length = 2

""" neosnippet
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-j>     <Plug>(neosnippet_expand_or_jump)
smap <C-j>     <Plug>(neosnippet_expand_or_jump)
xmap <C-j>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=
endif

" snippets directory
let g:neosnippet#snippets_directory = "~/dotfiles/snips"

" deoplete-biblatex
let g:deoplete#sources#biblatex#bibfile = "~/dzne/reports/thetext/bibliography.bib"
let g:deoplete#sources#biblatex#reloadbibfileonchange = 1
call deoplete#custom#source('biblatex', 'filetypes', ['rst', 'pandoc', 'markdown'])

""" jedi
let g:jedi#completions_enabled = 0
let g:jedi#show_call_signatures = "<leader>0"
autocmd FileType python set omnifunc=python3complete#Complete

""" Own augroup customizing file types
augroup jp
    autocmd!
    " autocmd FileType python set omnifunc=python3complete#Complete
    autocmd FileType markdown setlocal spell textwidth=80 list
    autocmd FileType make,automake setlocal noexpandtab list
    autocmd FileType gitcommit setlocal spell textwidth=76 colorcolumn=77
augroup END

" hope this helps with jedi-vim getting stuck autocompleting
" autocmd FileType python call jedi#configure_call_signatures()

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
" set number relativenumber
" augroup NumberSwitch
"     autocmd!
"     autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
"     autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
" augroup END

" theme settings
set cursorline
set noshowmode " if lightline installed
set background=dark
if has('gui_running')
    set guioptions-=T
    set guioptions-=r
    set guioptions-=R
    " linux subsystem on Windows beeps after error, this should disable it
    set belloff=all
    " let s:uname=system("uname")
    if g:os == "Darwin\n"
    	set guifont=Hack:h15
        let g:gruvbox_contrast_dark = "medium"
        colorscheme gruvbox
        set lines=70
        set columns=120
    elseif g:os == "Linux\n"
	    set guifont=Hack\ 11
        colorscheme Benokai
    elseif g:os == "Windows"
	    set guifont=Hack:h12
        colorscheme Benokai
        set lines=50
        set columns=90
    endif
    set guicursor+=a:blinkon0 " turn off blinking cursor
else
    if g:os == "Darwin\n"
        let g:gruvbox_contrast_dark = "medium"
        colorscheme gruvbox
    elseif g:os == "Linux\n"
        let g:gruvbox_contrast_dark = "medium"
        let g:gruvbox_italic = 1
        colorscheme gruvbox
    endif
endif

""" lightline setup
" Set colorscheme and
" Add git branch name to lightline using vim-fugitive
let g:lightline = {
    \ 'colorscheme': 'jellybeans',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
    \ },
    \ 'component_function': {
    \   'gitbranch': 'fugitive#head'
    \ },
    \ }

""" Text wrapping: wrap when the textwidth limit is reached
" I will put this here because I am not sure a plugin is messing with this
set textwidth=80    " Use gq to reformat (or gqip, gqq)
set colorcolumn=+1  " Relative to the textwidth variable instead of absolute
set nowrap

""" Goyo - distraction free writing
let g:goyo_width = 84
function! s:goyo_enter()
    if exists('$TMUX')
        silent !tmux set status off
        silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
    endif
    set noshowmode
    set noshowcmd
    " set scrolloff=999
endfunction

function! s:goyo_leave()
    if exists('$TMUX')
        silent !tmux set status on
        silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
    endif
    set showmode
    set showcmd
    " set scrolloff=5
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

""" Ale - linter
" ignore mypy for the time being
let g:ale_linters_ignore = {'python': ['mypy']}

""" Pandoc plugin
" Pretty text formatting using conceal
let g:pandoc#syntax#conceal#use = 1
let g:pandoc#syntax#codeblocks#embeds#langs = ['python', 'vim', 'make',
            \  'bash=sh', 'html', 'css', 'scss', 'javascript', 'yaml', 'r']
" somehow textwidth was reset at 0 when pandoc was loaded
" maybe because the formatting mode defaultes to soft
let g:pandoc#formatting#textwidth = 80
let g:pandoc#formatting#mode = "h"
" change folding mode so that all folds are displayed and not only
" the higher level headers
let g:pandoc#modules#disabled = ['folding']
let g:pandoc#folding#mode = 'none'
" from docs: enable pandoc functionality for markdown while using the markdown
" filetype and syntax
let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
let g:pandoc#filetypes#pandoc_markdown = 0
augroup pandoc_syntax
    au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
augroup END

let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Heading_L1',
        \ 'i:Heading_L2',
        \ 'k:Heading_L3'
    \ ]
    \ }

""" Vimgutter plugin
" shorter update times (default 4s)
set updatetime=100
