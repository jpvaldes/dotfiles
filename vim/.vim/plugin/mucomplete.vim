set completeopt+=menuone,noinsert,noselect
set complete=.,w,b,u
set shortmess+=c
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#chains = {
                  \ 'default':    ['file', 'keyn', 'omni', 'user', 'defs', 'c-n', 'uspl'],
                  \ 'vim':        ['file', 'keyn', 'cmd',  'omni', 'user', 'c-n', 'uspl'],
                  \ 'text':       ['file', 'c-n',  'uspl', 'omni', 'user'],
                  \ 'markdown':   ['file', 'c-n',  'uspl', 'omni', 'user'],
                  \ 'python':     ['nsnp', 'file'],
                  \ 'nim':        ['c-n', 'file', 'nsnp'],
                  \ }

" neosnippet (configuration from the documentation)
inoremap <silent> <expr> <plug><MyCR>
            \ mucomplete#neosnippet#expand_snippet("\<cr>")
imap <cr> <plug><MyCR>
