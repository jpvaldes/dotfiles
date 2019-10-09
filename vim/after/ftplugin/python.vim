let b:ale_linters = ['pyls']
let b:ale_fixers = ['yapf', 'isort']
let b:ale_linters_explicit = 1
let b:ale_fix_on_save = 1
let b:ale_python_pyls_config = {
            \ 'pyls': {
            \   'plugins': {
            \     'pylint' : {
            \       'enabled': v:false
            \     },
            \     'pydocstyle': {
            \       'enabled': v:false
            \     }
            \   }
            \  },
            \ }
