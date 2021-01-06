call plug#begin(stdpath('data') . '/plugged')

"Plug 'Olical/conjure', { 'for': 'clojure', 'do': 'bin/compile' }
"Plug 'Olical/vim-scheme', { 'for': 'scheme', 'on': 'SchemeConnect' }
Plug 'PeterRincker/vim-argumentative'
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
"Plug 'clojure-vim/acid.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'clojure-vim/async-clj-omni', { 'for': 'clojure' }
Plug 'ctrlpvim/ctrlp.vim'
Plug 'easymotion/vim-easymotion'
Plug 'embear/vim-localvimrc'
Plug 'guns/vim-clojure-highlight', { 'for': 'clojure'}
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
Plug 'guns/vim-sexp'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'liuchengxu/vim-better-default'
Plug 'liuchengxu/vim-clap'
Plug 'machakann/vim-highlightedyank'
Plug 'ncm2/float-preview.nvim'
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
"Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'radenling/vim-dispatch-neovim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'roxma/nvim-yarp'
Plug 'simnalamburt/vim-mundo'
Plug 'snoe/clj-refactor.nvim', { 'for': 'clojure', 'do': ':UpdateRemotePlugins' }
Plug 'srcery-colors/srcery-vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dadbod'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fireplace', { 'for': 'clojure', 'on': 'FireplaceConnect' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-tbone'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }
"Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'w0rp/ale'

call plug#end()

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect

"let g:python2_host_prog = '/home/linuxbrew/.linuxbrew/bin/python'
"let g:python3_host_prog = '/home/linuxbrew/.linuxbrew/bin/python3'
"let g:deoplete#enable_at_startup = 1
"call deoplete#custom#option('keyword_patterns', {'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*'})
"set completeopt-=preview

let g:float_preview#docked = 1
"let g:float_preview#docked = 0
"let g:float_preview#max_width = 80
"let g:float_preview#max_height = 40

let g:ale_linters = {
      \ 'clojure': ['clj-kondo', 'joker']
      \}
let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace'], }
let g:ale_fix_on_save = 1

let g:lightline = {
      \ 'colorscheme': 'srcery',
      "\ 'colorscheme': 'one',
      "\ 'colorscheme': 'darcula',
      "\ 'colorscheme': 'selenized_dark',
      \ }

nnoremap <leader>ff :Files<cr>
nnoremap <leader>fg :Rg<cr>
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>fc :Commands<cr>
nnoremap <leader>fh :Helptags<cr>
nnoremap <leader>fm :Maps<cr>
nnoremap <leader>f: :History:<cr>
nnoremap <leader>ft :Filetypes<cr>
nnoremap <leader>fr :History<cr>
nnoremap <leader>* :Rg <c-r><c-w><cr>

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --hidden -g "!.git/" --column --line-number --no-heading --color=never '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)
let g:airline_theme='base16_spacemacs'
let g:vim_better_default_persistent_undo = 1

runtime! plugin/default.vim

set undodir=~/.config/nvim/undo
set wrap
set clipboard-=unnamedplus
let g:clap_provider_grep_delay = 50
let g:clap_provider_grep_opts = '-H --no-heading --vimgrep --smart-case --hidden -g "!.git/"'

nnoremap <leader>* :Clap grep ++query=<cword><cr>
nnoremap <leader>fg :Clap grep<cr>
nnoremap <leader>ff :Clap files --hidden<cr>
nnoremap <leader>fb :Clap buffers<cr>
nnoremap <leader>fw :Clap windows<cr>
nnoremap <leader>fr :Clap history<cr>
nnoremap <leader>fh :Clap command_history<cr>
nnoremap <leader>fj :Clap jumps<cr>
nnoremap <leader>fl :Clap blines<cr>
nnoremap <leader>fL :Clap lines<cr>
nnoremap <leader>ft :Clap filetypes<cr>
nnoremap <leader>fm :Clap marks<cr>

map <leader>j <plug>(easymotion-prefix)
autocmd FileType clojure nnoremap <buffer> <localleader>re :Eval<cr>
autocmd FileType clojure vnoremap <buffer> <localleader>re :Eval<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>rf :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>rr :Require<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>rR :Require!<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>rt :RunTests<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>rl :Last<cr>
autocmd FileType clojure nnoremap <buffer> <localleader>rc :FireplaceConnect<cr>
autocmd FileType clojure nnoremap <buffer> gd :normal [<c-d><cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gp :Gpush<cr>
nnoremap <leader>gl :Gpull<cr>
nnoremap <leader>gf :Gfetch<cr>
nnoremap <leader>gcc :Gcommit --verbose<cr>
nnoremap <leader>gca :Gcommit --all --verbose<cr>
nnoremap <leader>gdl :diffget LOCAL<CR>
nnoremap <leader>gdr :diffget REMOTE<CR>
let g:localvimrc_persistent = 1
nnoremap <leader>ut :MundoToggle<cr>

"colorscheme space-vim-dark
colorscheme srcery

set termguicolors
set list
set spell
set wildmenu
set wildmode=list:longest,full
set mouse=a
set updatetime=100
set sessionoptions=blank,curdir,folds,help,tabpages,winsize
set inccommand=split

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

set number
set norelativenumber

