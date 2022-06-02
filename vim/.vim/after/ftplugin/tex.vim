let b:ale_linters = ['texlab', 'chktex', 'lacheck']
let b:ale_fixers = ['latexindent', 'remove_trailing_lines', 'trim_whitespace']
let b:ale_completion_enabled = 1
setl omnifunc=ale#completion#OmniFunc

