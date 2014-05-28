" markdown_bml.vim - Markdown preview
" Author: 
" Version: 1.0

if exists("g:loaded_markdown_bml_preview") || &cp || v:version < 700
    finish
endif
let g:loaded_markdown_bml_preview = 1

function! markdown_bml#preview()
    silent update
    let output_name = tempname() . '.html'

    let file_header = ['<html>', '<head>',
        \ '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">',
        \ '<title>'.expand('%s:p').'</title>',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssreset/reset-min.css">',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssbase/base-min.css">',
        \ '<link rel="stylesheet" type="text/css" href="http://yui.yahooapis.com/3.3.0/build/cssfonts/fonts-min.css">',
        \ '<style>body{padding:20px;}div#container{background-color:#F2F2F2;padding:0 20px;margin:0px;border:solid #D0D0D0 1px;}</style>',
        \ '</head>', '<body>', '<div id="container">']

    call writefile(file_header, output_name)

    silent exec '!markdown "' . expand('%:p') . '" >> "' . output_name . '"'
    silent exec '!echo "</div></body></html>" >> "' . output_name . '"'
    silent exec '!sensible-browser "' . output_name . '" &'
endfunction

inoremap <buffer> <F7> <ESC>:call markdown_bml#preview()<CR>
nmap <buffer> <F7> :call markdown_bml#preview()<CR>
