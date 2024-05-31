let b:ale_python_flake8_change_directory = 0
let b:ale_linters = ['pylsp', 'ruff']
let b:ale_fixers = ['ruff', 'isort', 'remove_trailing_lines', 'trim_whitespace']
let b:ale_fix_on_save = 1
let b:ale_completion_enabled = 1
setl omnifunc=ale#completion#OmniFunc

" ruff options
let b:ale_python_ruff_options = '--ignore F401'

" close the preview window when done
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" numbers
setlocal number
