set completeopt+=menuone,noinsert,noselect,preview
set complete=.,w,b,u
set shortmess+=c
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#chains = {
                  \ 'default':    ['file', 'keyn', 'omni', 'user', 'defs', 'c-n', 'uspl'],
                  \ 'vim':        ['file', 'keyn', 'cmd',  'omni', 'user', 'c-n', 'uspl'],
                  \ 'text':       ['file', 'c-n',  'uspl', 'omni', 'user'],
                  \ 'markdown':   ['file', 'c-n',  'uspl', 'omni', 'user'],
                  \ 'python':     ['omni', 'keyn', 'file'],
                  \ 'nim':        ['omni', 'keyn', 'file'],
                  \ }
