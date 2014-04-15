function! <SID>StripTrailingWhitespace()
    " save last search and cursor position
    let _s=@/
    let l = line(".")
    let c = col(".")
    " strip the trailing whitespace
    :%s/\s\+$//e
    " restore previous search history and cursor position
    let @/=_s
    call cursor(l, c)
endfunction
