function! SortLines(type, ...)
    let visual_mode = a:0
    let cmd = ""
    if visual_mode
        let cmd .= "'<,'>"
    else
        let cmd .= "'[,']"
    endif
    let cmd .= "sort " . get(g:, 'sort_lines_default_args', '')
    exe cmd
endfunction

nnoremap <leader>s :set opfunc=SortLines<CR>g@
vnoremap <leader>s :<C-U>call SortLines(visualmode(), 1)<CR>
