set encoding=utf-8

let g:mapleader=','

" we like the deep blues of Gotham
colorscheme gotham256

set ignorecase
set smartcase

" show matches and highlight searches
set showmatch
set hlsearch
nnoremap <leader><space> :nohlsearch<cr>

" save a keypress for ex commands
nnoremap ; :

" save us from VIM's default regex format
nnoremap / /\v
vnoremap / /\v

" allow viewing of whitespace characters
set showbreak=\\ " one space after the backslash
set listchars=tab:..,trail:_,nbsp:~,extends:>,precedes:<
nmap <leader>l :set list!<cr>

" delete trailing whitespace
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" wrap paragraphs
nnoremap <leader>q gqip

" split below and to the right
set splitbelow
set splitright

" split navigation
nnoremap <C-j> <C-W><C-J>
nnoremap <C-k> <C-W><C-K>
nnoremap <C-l> <C-W><C-L>
nnoremap <C-h> <C-W><C-H>

" split window vertically and switch to new window
nnoremap <leader>w <C-w>v<C-w>l

" toggle between absolute line numbers and relative line numbers
set number
set relativenumber
function! NumberToggle()
    if(&relativenumber == 1)
        set norelativenumber
        set number
    else
        set number
        set relativenumber
    endif
endfunc
nnoremap <leader>n :call NumberToggle()<cr>

" code folding
set foldmethod=indent
set foldlevel=99
"set foldnestmax=2
set foldenable
nnoremap <space> za

" setup Python 2 and Python 3 virtual enviroments
let g:python_host_prog = '/home/brendan/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '/home/brendan/.pyenv/versions/neovim3/bin/python'

" don't find precompiled Python (*.pyc) files when using wildcards
set wildignore+=*.pyc

" Python formatting a la PEP8
au BufNewFile,BufRead *.py
	\ set tabstop=4 |
	\ set softtabstop=4 |
	\ set shiftwidth=4 |
	\ set textwidth=100 |
	\ set expandtab |
	\ set autoindent |
	\ set fileformat=unix

au BufNewFile,BufRead *.py,*.pyw,*.c,*.h,*.cpp,*.hpp match Cursor /\s\+$/

au BufNewFile,BufRead *js,*.html,*.css
	\ set tabstop=2 |
	\ set softtabstop=2 |
	\ set shiftwidth=2 |
	\ set expandtab
