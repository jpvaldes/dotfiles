setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

compiler nim

" let g:ale_nim_nimlsp_nim_sources = expand('~/.nimble/bin/nim')
let b:ale_linters = ['nimlsp']
let b:ale_fixers = ['nimpretty']
let b:ale_lsp_root = '.'
