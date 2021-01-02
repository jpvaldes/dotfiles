" Do NOT initiate ALE when doing short-lived diffs.
if &diff
    finish
endif

let g:ale_sign_error = '✖'
let g:ale_sign_info = '⚠️'
let g:ale_sign_warning = 'W'


" Mappings
nmap <silent> <Leader>a <Plug>(ale_next_wrap)zz
nmap <silent> <Leader>A <Plug>(ale_previous_wrap)zz
" Open location list
nnoremap <leader>l :lopen<CR>
