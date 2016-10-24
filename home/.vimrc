set nocompatible
filetype off

set t_Co=256

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()


" Plugin 'VundleVim/Vundle.vim' handled by homeshick
Plugin 'noahfrederick/vim-skeleton'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-haml'
Plugin 'fatih/vim-go'
Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'airblade/vim-gitgutter'
Plugin 'rking/ag.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'tmhedberg/matchit'
Plugin 'mattn/emmet-vim'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'ciaranm/detectindent'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ervandew/supertab'
Plugin 'vim-scripts/ZoomWin'

" needs ctags installed
Plugin 'majutsushi/tagbar'

call vundle#end()
filetype plugin indent on

syntax on

set mouse=nv
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
map <F3> :NERDTreeToggle<CR>
map <F4> :ZoomWin<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tagbar
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>] :TagbarToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Nerdtree ignores
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let NERDTreeIgnore = ['\.pyc$', '__pycache__']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git anyway...
set nobackup
set nowb
set noswapfile

set undodir=~/.vim/undodir
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <leader><space> :noh<cr>
map <leader>x :ccl<cr>

" Template insertion
nnoremap <leader>t :SkelInsert!<cr>

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
cnoreabbrev ag Ag!
cnoreabbrev Ag Ag!

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

map <leader>zz %:sleep 1000m<CR>%

"" YouCompleteMe
nnoremap <leader>gd :YcmCompleter GoToDeclaration<cr>

" make YCM use the python version of the virtualenv to do completions
let g:ycm_python_binary_path = split(system("which python"))[0]

"SuperTab
let g:SuperTabDefaultCompletionType = "<c-n>"

" Ctrl P
let g:ctrlp_map = '<c-p>'
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

set wildignore=vendor "ignoring all the gems bundled

autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif


" Go mappings

au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)

" Vim-Airline

let g:airline_left_sep='|'
let g:airline_right_sep='|'
let g:airline#extensions#branch#empty_message = 'no-git'
let g:airline_theme='dark'

" Theme
set background=dark
colorscheme grb256

" Markdown, not Modula-2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufWritePost *.md :silent !markdown -o <afile>:p:h/<afile>:t:r.html <afile>:p

" Remove trailing whitespace from certain files
autocmd FileType c,cpp,java,php,python autocmd BufWritePre <buffer> :%s/\s\+$//e

" Show different background after 80 columns
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#2c2d27


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RUNNING TESTS taken from Gary Bernhardt
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader><cr> :call RunTestFile()<cr>
nnoremap <leader>T :call RunNearestTest()<cr>
nnoremap <leader>a :call RunTests('')<cr>
nnoremap <leader>c :w\|:!script/features<cr>
nnoremap <leader>w :w\|:!script/features --profile wip<cr>

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Run the tests for the previously-marked file.
    let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|test_.\+.py\)$') != -1
    if in_test_file
        call SetTestFile(command_suffix)
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file)
endfunction

function! RunNearestTest()
    let spec_line_number = line('.')
    call RunTestFile(":" . spec_line_number)
endfunction

function! SetTestFile(command_suffix)
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@% . a:command_suffix
endfunction

function! RunTests(filename)
    " Write the file and run tests for the given filename
    if expand("%") != ""
      :w
    end
    if match(a:filename, '\.feature$') != -1
        exec ":!script/features " . a:filename
    else
        " First choice: project-specific test script
        if filereadable("script/test")
            exec ":!script/test " . a:filename
        " Fall back to the .test-commands pipe if available, assuming someone
        " is reading the other side and running the commands
        elseif filewritable(".test-commands")
          let cmd = 'rspec --color --format progress --require "~/lib/vim_rspec_formatter" --format VimFormatter --out tmp/quickfix'
          exec ":!echo " . cmd . " " . a:filename . " > .test-commands"

          " Write an empty string to block until the command completes
          sleep 100m " milliseconds
          :!echo > .test-commands
          redraw!
        " Fall back to a blocking test run with Bundler
        elseif filereadable("Gemfile")
            exec ":!bundle exec rspec --color " . a:filename
        " If we see python-looking tests, assume they should be run with Nose
        elseif strlen(glob("test/**/*.py") . glob("**/tests/**/*.py"))
            exec "!py.test -s " . a:filename
        " Fall back to a normal blocking test run
        else
            exec ":!rspec --color " . a:filename
        end
    end
endfunction
