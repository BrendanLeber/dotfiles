function TrailingWhiteSpace()
    highlight WhiteSpaceEOL term=reverse ctermbg=white guibg=white
    match WhiteSpaceEOL /\s\+$/
endfunction

au BufNewFile,BufReadPost,WinEnter * call TrailingWhiteSpace()
