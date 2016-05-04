scriptencoding utf-8
set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'tmhedberg/SimpylFold'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'bling/vim-bufferline'
Plugin 'majutsushi/tagbar'
Plugin 'easymotion/vim-easymotion'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-vinegar'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-tbone'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-obsession'
Plugin 'tpope/vim-speeddating'
Plugin 'phillipberndt/python-imports.vim'
Plugin 'ervandew/supertab'
Plugin 'jiangmiao/auto-pairs'
"Plugin 'jmcantrell/vim-virtualenv'
"Plugin 'lambdalisue/vim-pyenv' " pip install pyenv
Plugin 'mbbill/undotree'

" These take up a lot of space
"Plugin 'klen/python-mode' " DISABLED
Plugin 'davidhalter/jedi-vim' " pip install jedi

" Colours
Plugin 'jnurmine/Zenburn'
Plugin 'sjl/badwolf'
Plugin 'tomasr/molokai'
Plugin 'fmoralesc/vim-vitamins'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ciaranm/inkpot'
Plugin 'nanotech/jellybeans.vim'
Plugin 'baskerville/bubblegum'
Plugin 'morhetz/gruvbox'
Plugin 'w0ng/vim-hybrid'
Plugin 'mitsuhiko/fruity-vim-colorscheme'
" Plugin 'chriskempson/base16-vim'

" TODO: re-enable
"Plugin 'scrooloose/syntastic'
"Plugin 'Valloric/YouCompleteMe'

" Only need these when python-mode is disabled
Plugin 'hynek/vim-python-pep8-indent'
Plugin 'hdima/python-syntax'
Plugin 'nvie/vim-flake8' " pip install flake8

call vundle#end()
filetype plugin indent on

set encoding=utf-8
syntax on

set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

nnoremap <C-c> <silent> <C-c>

inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>0

set foldmethod=indent
set foldlevel=99
let g:SimpylFold_docstring_preview=1

au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix |
    \ let g:SuperTabDefaultCompletionType = "context" |
    \ let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"

augroup vimrc_autocmds
    autocmd!
    autocmd FileType python highlight Excess ctermbg=Red guibg=Red
    autocmd FileType python match Excess /\%79v.*/
    autocmd FileType python set nowrap
augroup END

if has('gui_running')
    set background=dark
    colorscheme solarized
else
    set t_Co=256
    colorscheme jellybeans
    "colorscheme fruity
    "colorscheme solarized
    "colorscheme inkpot
    "colorscheme zenburn
    "colorscheme molokai
    "colorscheme badwolf
    "colorscheme bubblegum-256-dark
endif
"call togglebg#map("<F9>")

" override colours on some systems
if hostname() =~ '^wolfman'
    let g:gruvbox_termcolors=16
    colorscheme gruvbox
elseif hostname() =~ '^CHESTER'
    colorscheme fruity
endif

set number

nmap <leader>t <ESC>:TagbarToggle<cr>
imap <leader>t <ESC>:TagbarToggle<cr>i

"let g:ycm_autoclose_preview_window_after_completion=1
"map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>

let g:pydiction_location = '/home/user/.vim/bundle/pydiction/complete-dict'
set laststatus=2
" set showmode
set noshowmode " turn this off for airline

set ruler
set encoding=utf-8
set showcmd
set scrolloff=2
"set autoindent
"set smartindent
"set cindent
inoremap # X<BS>#
set mouse=
"if has('mouse')
"  set mouse=a
"endif
set incsearch
set ignorecase
set smartcase
set cino=>4
set showmatch
set hlsearch
" autocmd BufRead,BufNewFile *.lsp,*.lisp so ~/VIlisp-2.0/VIlisp.vim | set bufhidden=hide
nmap <silent> <F3> :silent nohlsearch<CR>
imap <silent> <F3> <C-o>:silent nohlsearch<CR>
" colorscheme inkpot
" colorscheme darkblue
nmap <silent> <F4> :silent setlocal spell spelllang=en_gb<CR>
imap <silent> <F4> <C-o>:silent setlocal spell spelllang=en_gb<CR>
nmap <silent> <F5> :silent setlocal nospell<CR>
imap <silent> <F5> <C-o>:silent setlocal nospell<CR>

nmap <silent> <F2> :silent set diffopt+=iwhite<CR>
imap <silent> <F2> <C-o>:silent set diffopt+=iwhite<CR>

" on some systems this is not detected correcly
set background=dark

autocmd BufRead *.upc set filetype=c
autocmd BufRead *.gnu set filetype=gnuplot

autocmd FileType c,cpp set cindent

if !exists("autocommands_loaded")
  let autocommands_loaded = 1
  augroup C
      autocmd BufRead *.c set cindent
  augroup END
endif

" gnu indentation style
" augroup C
"    autocmd BufRead *.c set cinoptions={.5s,:.5s,+.5s,t0,g0,^-2,e-2,n-2,p2s,(0,=.5s formatoptions=croql cindent shiftwidth=4 tabstop=8
" augroup END

" Show tabs and trailing whitespace visually
"if (&termencoding == "utf-8") || has("gui_running")
"    if v:version >= 700
"        set list listchars=tab:»·,trail:·,extends:…,nbsp:‗
"    else
"        set list listchars=tab:»·,trail:·,extends:…
"    endif
"else
    if v:version >= 700
        set list listchars=tab:>-,trail:.,extends:>,nbsp:_
    else
        set list listchars=tab:>-,trail:.,extends:>
    endif
"endif

" Enable modelines only on secure vim versions
if (v:version >= 604)
    set modeline
else
    set nomodeline
endif
set tabstop=4
set expandtab
set shiftwidth=4
set shiftround
set matchpairs+=<:>

if has('gui')
    set guioptions-=m
    set guioptions-=T
    set guioptions-=l
    set guioptions-=L
    set guioptions-=r
    set guioptions-=R
    "set number
end

" Nice statusbar - replaced by airline
"set laststatus=2
"set statusline=
"set statusline+=%2*%-3.3n%0*\                " buffer number
"set statusline+=%f\                          " file name
"set statusline+=%h%1*%m%r%w%0*               " flags
"set statusline+=\[%{strlen(&ft)?&ft:'none'}, " filetype
"set statusline+=%{&encoding},                " encoding
"set statusline+=%{&fileformat}]              " file format
"set statusline+=%=                           " right align
"set statusline+=%2*0x%-8B\                   " current char
"set statusline+=%-14.(%l,%c%V%)\ %<%P        " offset
"if filereadable(expand("$VIM/vimfiles/plugin/vimbuddy.vim"))
"    set statusline+=\ %{VimBuddy()}          " vim buddy
"endif

if has('title') && (has('gui_running') || &title)
    set titlestring=
    set titlestring+=%f\                     " file name
    set titlestring+=%h%m%r%w                " flags
    set titlestring+=\ -\ %{v:progname}      " program name
endif

" If possible, try to use a narrow number column.
if v:version >= 700
    try
        setlocal numberwidth=3
    catch
    endtry
endif

" Filter expected errors from make
"if has("eval") && v:version >= 700
"    let &makeprg='nice make $* 2>&1 \| sed -u -n '
"    let &makeprg.='-e "/should fail/s/:\([0-9]\)/∶\1/g" '
"    let &makeprg.='-e "/usr.share.aclocal.*underquoted/s/:\([0-9]\)/∶\1/g" '
"    let &makeprg.='-e "s/\([0-9]\{2\}\):\([0-9]\{2\}\):\([0-9]\{2\}\)/\1∶\2∶\3/g" '
"    let &makeprg.='-e "/-version-info/s/:\([0-9]\)/∶\1/g" '
"    let &makeprg.='-e "/^/p" '
"endif

" autocmd FileType tex set makeprg=pdflatex\ %\ $*
" autocmd FileType bib set makeprg=bibtex\ %\ $*

nmap q: :q

" tab completion - taken from ciaranm's vimrc
"if has("eval")
"    function! CleverTab()
"        if strpart(getline('.'), 0, col('.') - 1) =~ '^\s*$'
"            return "\<Tab>"
"        else
"            return "\<C-N>"
"        endif
"    endfun
"    inoremap <Tab> <C-R>=CleverTab()<CR>
"    inoremap <S-Tab> <C-P>
"endif

if has("autocmd")
    au VimEnter * nohls
endif

set backspace=indent,eol,start
set showfulltag
set lazyredraw
"set noerrorbells
set visualbell

set wildmenu
set wildignore+=*.o,*~,.lo
set suffixes+=.in,.a

set hidden

set winminheight=1

autocmd FileType perl set makeprg=perl\ -c\ %\ $*
autocmd FileType perl set errorformat=%f:%l:%m
autocmd FileType perl set autowrite

set viminfo='1000,f1,:1000,/1000
set history=500

set backup
"set patchmode=.clean

set dictionary=/usr/share/dict/words
"set complete=.,w,u,t,i,d,k
set complete=.,w,u,t,i,d

"autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
"autocmd BufRead *.py set nocindent
"autocmd BufWritePre *.py normal m`:%s/\s\+$//e ``

"let g:syntastic_python_checkers = ['pyflakes']
"let g:syntastic_python_python_exec = 'python3'
let g:syntastic_check_on_open = 0
let g:syntastic_disabled_filetypes=['python'] " use jedi instead

set colorcolumn=80
"set cursorline
nnoremap <silent> <Leader>l ml:execute 'match Search /\%'.line('.').'l/'<CR>
nnoremap <silent> <Leader>m ml:execute 'match'<CR>

nnoremap <silent> <leader>c :execute 'sign unplace * buffer=' . bufnr('')<CR>
nmap <leader>C <ESC>:lclose<CR>
nmap <leader>x <ESC>:cclose<CR>

nmap <leader><tab> <ESC>:bnext<CR>

noremap <F8> :PymodeLintAuto<CR>

" python-mode config, hopefully without jedi-vim conflicts - BUNDLE DISABLED
"let g:pymode_rope = 0
"let g:pymode_rope_completion = 0
"let g:pymode_indent = 1
"let g:pymode_doc = 0
""let g:pymode_doc_bind = '<leader>K'
""let g:pymode_rope_goto_definition_bind = '<leader>g'
"let g:pymode_lint = 1
"let g:pymode_lint_write = 1
""let g:pymode_lint_checker = "pyflakes,pep8,pylint,pep257,mccabe"
"let g:pymode_lint_checker = "pyflakes,pep8"
"let g:pymode_lint_message = 1
"let g:pymode_lint_on_fly = 0
"let g:pymode_lint_cwindow = 1
"let g:pymode_virtualenv = 1
"let g:pymode_breakpoint = 1
"let g:pymode_breakpoint_bind = '<leader>B'
"let g:pymode_run_bind = '<leader>R'
"let g:pymode_syntax = 1
"let g:pymode_syntax_all = 1
"let g:pymode_syntax_indent_errors = g:pymode_syntax_all
"let g:pymode_syntax_space_errors = g:pymode_syntax_all
"let g:pymode_rope_autoimport = 1
"let g:pymode_rope_autoimport_import_after_complete = 1

" if we're using hdima/python-syntax instead of python mode
let python_highlight_all=1
" if we're using nvie/vim-flake8 instead of python mode
let g:flake8_show_quickfix=1
let g:flake8_show_in_gutter=1
let g:flake8_show_in_file=1
"autocmd BufWritePost *.py call Flake8()
autocmd FileType python map <buffer> <F6> :call flake8#Flake8UnplaceMarkers()<CR>


" doesn't work with python3 (no activate_this.py)
"py << EOF
"import os.path
"import sys
"import vim
"if 'VIRTUAL_ENV' in os.environ:
"    project_base_dir = os.environ['VIRTUAL_ENV']
"    sys.path.insert(0, project_base_dir)
"    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
"    execfile(activate_this, dict(__file__=activate_this))
"EOF

" disable this on slow systems
let g:jedi#popup_on_dot = 0

if has('gui_running')
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#buffer_nr_show = 1
endif
let g:airline#extensions#bufferline#enabled = 1
"let g:airline#extensions#bufferline#overwrite_variables = 1

map <silent> <C-E> :silent Lexplore<CR>
if has('gui_running')
    let g:netrw_liststyle = 3
else
    let g:netrw_liststyle = 0
endif
let g:netrw_winsize = -20
let g:netrw_browse_split = 4
let g:netrw_banner = 0
let g:netrw_altv = 1
let g:netrw_preview = 1
let g:netrw_sort_sequence = '[\/]$,*'
let g:netrw_chgwin=2
set autochdir

" vim: set shiftwidth=4 softtabstop=4 expandtab tw=72                  :
