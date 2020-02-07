set termguicolors
" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
let g:deoplete#enable_at_startup = 1
call plug#begin('~/.local/share/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' } " File browser
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'christoomey/vim-tmux-navigator' " Tmux integration
Plug 'vim-airline/vim-airline' " Nice status line
Plug 'vim-airline/vim-airline-themes' " themes for airline
Plug 'scrooloose/nerdcommenter' " Comments handling
Plug 'airblade/vim-gitgutter' " Git status display
Plug 'Elkasitu/vim-pudb' " Connection to pudb breakpoints
Plug 'iCyMind/NeoSolarized' " Theme
Plug 'junegunn/fzf.vim' " Fuzzy file search
Plug 'tpope/vim-surround' " vim surround
Plug 'tpope/vim-repeat' " extended repeat
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python3'

filetype plugin indent on

syntax on

set mouse=""
set nowrap
set nocompatible
set ls=2
set tabstop=4
set softtabstop=4
set shiftwidth=4
set showmode
set incsearch
set ruler
set number
set ignorecase
set smartcase
set ttyfast
set so=7 " Set 7 lines to the cursor when moving vertical
set ss=1
set siso=10
set listchars+=precedes:<,extends:>
set backspace=eol,start,indent

set expandtab
set sm


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Nerdtree 
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <F3> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$', '__pycache__']

" Turn backup off, since most stuff is in SVN, git anyway...
set nobackup
set nowb
set noswapfile

set undodir=~/.local/share/nvim/undodir
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <leader><space> :noh<cr>
map <leader>x :ccl<cr>

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

set clipboard=unnamedplus
set pastetoggle=<F2>
nnoremap ; :

" fzf
cnoreabbrev ag Ag

fun! FzfOmniFiles()
    let is_git = system('git status')
    if v:shell_error
        :Files
    else
        :GitFiles --others --exclude-standard --cached
    endif
endfun
map <c-p> :call FzfOmniFiles()<cr>

let g:rg_command = 'rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --color "always" -g "!{.git}/*" '

command! -bang -nargs=* Ag call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)

map <leader>zz %:sleep 1000m<CR>%

" Markdown, not Modula-2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
"
" Remove trailing whitespace from certain files
autocmd FileType c,cpp,java,php,python autocmd BufWritePre <buffer> :%s/\s\+$//e

" Theme

let g:neosolarized_visibility = "high"
colorscheme NeoSolarized
set background=light

map <Leader>b :let &background = ( &background == "dark"? "light" : "dark" )<CR>

" Vim-Airline

let g:airline_left_sep='|'
let g:airline_right_sep='|'
let g:airline#extensions#branch#empty_message = 'no-git'
let g:airline_theme='solarized'


" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   execute "normal! g`\"" |
    \ endif


" Underline current line
nnoremap <leader>u YpVr

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ale
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:ale_python_mypy_options = '--follow-imports normal --ignore-missing-imports'

" COC related settings

set hidden

set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

nmap <leader>qf  <Plug>(coc-fix-current)

" Use `:Format` to format current buffer
nmap <leader>f :call CocAction('format')<CR>
