setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

compiler nim

set omnifunc=ale#completion#OmniFunc
let b:ale_linters = ['nimcheck']
let b:ale_fixers = ['nimpretty', 'remove_trailing_lines', 'trim_whitespace']
" let b:ale_lsp_root = '.'
let b:ale_fix_on_save = 0
