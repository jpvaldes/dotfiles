set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath=&runtimepath
let g:python3_host_prog=expand('~/miniconda/envs/neovim37/bin/python3.7')
source ~/.vimrc
