" Do NOT initiate ALE when doing short-lived diffs.
if &diff
    finish
endif

let g:ale_sign_error = '✖'
let g:ale_sign_warning = '⚠️'
let g:ale_sign_info = 'I'
highlight ALEErrorSign ctermbg =NONE ctermfg=red
highlight ALEWarningSign ctermbg =NONE ctermfg=yellow
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 1
let g:ale_lint_on_save = 1

" Mappings
nmap <silent> <Leader>a <Plug>(ale_next_wrap)zz
nmap <silent> <Leader>A <Plug>(ale_previous_wrap)zz
