filetype off
if !exists("g:loaded_pathogen")
    call pathogen#infect()
    call pathogen#helptags()
endif
filetype plugin indent on

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" stop backup files from being created.  we use source control
set nobackup
set noswapfile

" everything should be UTF-8 when reading or writing to files
set encoding=utf-8
set fenc=utf-8
set termencoding=utf-8

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set history=50      " keep 50 lines of command line history
set ruler           " show the cursor position all the time
set showcmd         " display incomplete commands
set incsearch       " do incremental searching

set relativenumber
set visualbell
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

" rapidly toggle `set list`
nmap <leader>l :set list!<CR>

" use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

syntax enable
set t_Co=256
set t_ut=
"set background=dark
colorscheme inkpot

set ttyfast

" some searching improvements
nnoremap \ /\v
vnoremap \ /\v
set ignorecase
set smartcase
set gdefault
set showmatch
set hlsearch

" Don't use Ex mode, use Q for formatting
" map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
" inoremap <C-U> <C-G>u<C-U>

" Only do this part when compiled with support for autocommands.
if has("autocmd")
    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent indenting.
    filetype plugin indent on

    " Put these in an autocmd group, so that we can delete them easily.
    " augroup vimrcEx
    " au!
    "
    " " For all text files set 'textwidth' to 78 characters.
    autocmd FileType text setlocal textwidth=78
else

    set autoindent        " always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
" if !exists(":DiffOrig")
"   command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
"         \ | wincmd p | diffthis
" endif

let mapleader=","
nmap <leader>v :source ~/.vimrc<cr>
nmap <leader>V :e ~/.vimrc<cr>
noremap <leader>h :nohl<cr>

noremap <leader>ls :!ls $(dirname %)<cr>
noremap <leader>la :!ls -al $(dirname %)<cr>

" switch between windows easier
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

function! Preserve(command)
    " save last search and cursor position
    let _s=@/
    let l = line(".")
    let c = col(".")
    " do the business
    execute a:command
    " restore previous search history and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

" remove trailing whitespace
nmap _$ :call Preserve("%s/\\s\\+$//e")<CR>

" reindent entire file
nmap _= :call Preserve("normal gg=G")<CR>

" read in Isilon specific settings if it exists
if filereadable("~/.isilon.vim")
    source ~/.isilon.vim
endif
