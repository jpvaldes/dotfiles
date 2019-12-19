" Disable insert comment after o or O, use gqq to format a long comment into
" several lines or let vim just take you to the next line
set formatoptions-=o

" Disable insert comment after new line in insert mode
set formatoptions-=r

" K calls :help command for keyword in vim file
" reference: https://bre.is/WC3Ih-26u
set keywordprg=:help
