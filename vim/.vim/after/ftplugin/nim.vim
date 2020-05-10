setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

compiler nim

set omnifunc=ale#completion#OmniFunc
let b:ale_linters = ['nimlsp']
let b:ale_fixers = ['nimpretty']
let b:ale_lsp_root = '.'
