let b:ale_python_pyls_executable = 'pyls'
let b:ale_python_pyls_config = {
            \ 'pyls': {
            \   'configurationSources': ['flake8'],
            \   'plugins': {
            \     'flake8': {
            \       'enabled': v:true
            \     },
            \     'pylint' : {
            \       'enabled': v:false
            \     },
            \     'pydocstyle': {
            \       'enabled': v:false
            \     },
            \     'pycodestyle': {
            \       'enabled': v:false
            \     },
            \     'mccabe': {
            \       'enabled': v:false
            \     },
            \     'autopep8': {
            \       'enabled': v:false
            \     },
            \     'pyflakes': {
            \       'enabled': v:false
            \     },
            \     'yapf': {
            \       'enabled': v:false
            \     },
            \     'rope_completion': {
            \       'enabled': v:false
            \     },
            \     'rope_rename': {
            \       'enabled': v:false
            \     },
            \   }
            \  },
            \ }
let b:ale_linters = ['flake8', 'mypy']
let b:ale_fixers = ['yapf', 'isort']
let b:ale_linters_explicit = 1
let b:ale_fix_on_save = 1
