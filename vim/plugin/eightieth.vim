function Eightieth()
    hi Eightieth term=reverse ctermbg=red guibg=red
    match Eightieth /\%80v/
endfunction

autocmd BufNewFile,BufReadPost *.c,*.h call Eightieth()
