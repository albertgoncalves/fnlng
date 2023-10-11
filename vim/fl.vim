" $ cp fl.vim ~/.vim/syntax/fl.vim
" $ grep '.fl' ~/.vimrc
" autocmd BufNewFile,BufRead *.fl setlocal filetype=fl

if exists("b:current_syntax")
    finish
endif

syn match Operator  "\([(){}\[\]|,:;]\)\|\([\\=+\-]\)\|\(:=\)\|\(::\)"
syn match Number    "\<[0-9]\+\>"
syn match Keyword   "\<_\>"

syn keyword Keyword     main print
syn keyword Conditional if else match

syn keyword Todo FIXME NOTE TODO contained
syn match Comment "#.*" contains=Todo

let b:current_syntax = "fl"
