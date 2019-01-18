" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.local/share/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' } " File browser
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'w0rp/ale' " Syntax checking
Plug 'Valloric/YouCompleteMe' " Code completion
Plug 'christoomey/vim-tmux-navigator' " Tmux integration
Plug 'ervandew/supertab' " Smart tab key
Plug 'bling/vim-airline' " Nice status line
Plug 'scrooloose/nerdcommenter' " Comments handling
Plug 'airblade/vim-gitgutter' " Git status display
Plug 'Elkasitu/vim-pudb' " Connection to pudb breakpoints
Plug 'altercation/vim-colors-solarized' " Theme
Plug 'junegunn/fzf.vim' " Fuzzy file search
Plug 'sudar/vim-arduino-syntax' " Arduino syntax highlighting
Plug 'tpope/vim-surround' " vim surround
Plug 'digitaltoad/vim-pug'  " pug js templating
Plug 'posva/vim-vue'  " vue plugin
" Dependency; required for vim-syncopate.
Plug 'google/vim-maktaba'
Plug 'google/vim-syncopate'
Plug 'google/vim-glaive'

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

call glaive#Install()
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syncopate
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Glaive syncopate browser='chromium'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ale
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:ale_python_mypy_options = '--follow-imports normal --ignore-missing-imports'

nnoremap <leader>f :ALEFix<cr>

let g:ale_fixers = {
\   'python':['yapf', 'isort'],
\   'javascript':['eslint'],
\   'vue':['eslint'],
\}


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" YouCompleteMe
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>gd :YcmCompleter GoToDeclaration<cr>
" autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" make YCM use the python version of the virtualenv to do completions
"let g:ycm_python_binary_path = 'python'
" let g:ycm_log_level = "debug"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Nerdtree 
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <F3> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$', '__pycache__']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SuperTab
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:SuperTabDefaultCompletionType = "<c-n>"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

" Vim-Airline

let g:airline_left_sep='|'
let g:airline_right_sep='|'
let g:airline#extensions#branch#empty_message = 'no-git'
let g:airline_theme='dark'

" Theme
set t_Co=256
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"
set background=dark
colorscheme solarized

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   execute "normal! g`\"" |
    \ endif


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
    let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|test_.\+.py\|.\+_test.py\)$') != -1
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
        elseif strlen(glob("**/tests/**/*.py"))
            exec "!TOX_ENV=simple make ". a:filename
        " Fall back to a normal blocking test run
        else
            exec ":!rspec --color " . a:filename
        end
    end
endfunction

" Underline current line
nnoremap <leader>u YpVr

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
