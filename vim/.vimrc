set nocompatible
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html

" Own plugins
Plugin 'Elive/vim-colorscheme-elive'
Plugin 'lifepillar/vim-solarized8'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'morhetz/gruvbox'

Plugin 'scrooloose/nerdtree'
" Plugin 'kien/ctrlp.vim'
" Plugin 'andviro/flake8-vim'
" Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'kshenoy/vim-signature'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'
Plugin 'hari-rangarajan/CCTree'
Plugin 'terryma/vim-expand-region'
Plugin 'dhruvasagar/vim-table-mode'
Plugin 'fatih/vim-go'
Plugin 'dense-analysis/ale'
Plugin 'roxma/nvim-yarp'
Plugin 'roxma/vim-hug-neovim-rpc'
Plugin 'AndrewRadev/splitjoin.vim'
" Plugin 'mdempsky/gocode', {'rtp': 'vim/'}
Plugin 'tpope/vim-surround'
Plugin 'airblade/vim-gitgutter'

" Yaml plugin
Plugin 'mrk21/yaml-vim' 

" Snippets
Plugin 'Shougo/deoplete.nvim'
Plugin 'Shougo/neosnippet.vim'
Plugin 'Shougo/neosnippet-snippets'

" Autocompletion
" Plugin 'Valloric/YouCompleteMe'
" Plugin 'Shougo/deoplete.nvim'
" Plugin 'deoplete-plugins/deoplete-jedi'

" Python plugins
" Plugin 'vim-scripts/indentpython.vim'
" Plugin 'tell-k/vim-autopep8'

" Auto-close
Plugin 'Townk/vim-autoclose'

" FZF
Plugin 'junegunn/fzf.vim'

" org mode
Plugin 'jceb/vim-orgmode'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins " :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate " :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

syntax enable
" set t_Co=256
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set termguicolors
" colorscheme elive
set background=dark
colorscheme gruvbox

" Misc {{{
set ttyfast                     " faster redraw
set backspace=indent,eol,start
set mouse=a
set encoding=utf-8
" }}}

" Spaces & Tabs {{{
set tabstop=4           " 4 space tab
set expandtab           " use spaces for tabs
set softtabstop=4       " 4 space tab
set shiftwidth=4
set modelines=1
filetype indent on
filetype plugin on
set autoindent
" }}}

" " UI Layout {{{
set number              " show line numbers
set showcmd             " show command in bottom bar
set cursorline          " highlight current line
set wildmenu
set lazyredraw
set showmatch           " higlight matching parenthesis
" }}}

" Searching {{{
set ignorecase          " ignore case when searching
set incsearch           " search as characters are entered
set hlsearch            " highlight all matches

" NerdTree
" autocmd VimEnter * NERDTree
" autocmd BufWinEnter * NERDTreeMirror
" autocmd VimEnter * wincmd p
map <F5> :NERDTreeFocus<CR>

" TagBar
 nnoremap <silent> <F9> :TagbarToggle<CR>

" CtrlP settings
" let g:ctrlp_match_window = 'bottom,order:ttb'
" let g:ctrlp_switch_buffer = 0
" let g:ctrlp_working_path_mode = 0

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
  
  " bind \ (backward slash) to grep shortcut
  command -nargs=+ -complete=file -bar Ag silent! grep! <args>|botright cwindow|redraw!
  nnoremap \ :Ag<SPACE>
endif

" CScope
if has('cscope')
	set cscopetag cscopeverbose

	if has('quickfix')
		set cscopequickfix=s-,c-,d-,i-,t-,e-
	endif

	cnoreabbrev csa cs add
	cnoreabbrev csf cs find
	cnoreabbrev csk cs kill
	cnoreabbrev csr cs reset
	cnoreabbrev css cs show
	cnoreabbrev csh cs help

	command -nargs=0 Cscope cs add $VIMSRC/src/cscope.out $VIMSRC/src
endif

" Automatically load cscope.out in current directory
function! LoadCscope()
    let db = findfile("cscope.out", ".;")
    if (!empty(db))
        let path = strpart(db, 0, match(db, "/cscope.out$"))
        set nocscopeverbose " suppress 'duplicate connection' error
        exe "cs add " . db . " " . path
        " exe "CCTreeLoadDB " . db
        set cscopeverbose
    endif
endfunction
au BufEnter /* call LoadCscope()


" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" Easy split navigations
nnoremap <Space>j <C-W><C-J>
nnoremap <Space>k <C-W><C-K>
nnoremap <Space>l <C-W><C-L>
nnoremap <Space>h <C-W><C-H>

" Navigate in insert mode without arrow keys
inoremap <c-k> <up>
inoremap <c-j> <down>
inoremap <c-h> <left>
inoremap <c-l> <right>

" Ctrl-P
" nnoremap <Space>p :CtrlP<CR>

" Leader settings
let mapleader = ","
" nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P
nmap <Leader><Leader> V
nmap <leader>n :NERDTreeToggle<cr>
nmap <leader>r :!%:p<cr>

" Resize panels
nnoremap <silent> <Leader>+ :exe "resize +5"<CR>
nnoremap <silent> <Leader>- :exe "resize -5"<CR>
nnoremap <silent> <Leader>> :exe "vertical resize +5"<CR>
nnoremap <silent> <Leader>< :exe "vertical resize -5"<CR>

" Stay in normal mode after inserting new line
nnoremap o o<Esc>
nnoremap O O<Esc>

" vim-session settings
let g:session_autosave = 'yes'
let g:session_autosave_periodic = 1

" Avoid the escape key
imap ii <Esc>

" vim-airline customization
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" vim-go customization
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)

au FileType go set noexpandtab
au FileType go set shiftwidth=4
au FileType go set softtabstop=4
au FileType go set tabstop=4

let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:go_auto_sameids = 1
let g:go_auto_type_info = 1
set updatetime=100

"" Autoimport dependencies
let g:go_fmt_command = "goimports"

" Add snippet engine
let g:go_snippet_engine = "neosnippet"

"" Error and warning signs.
let g:ale_sign_error = '⤫'
let g:ale_sign_warning = '⚠'

"" Enable integration with airline.
let g:airline#extensions#ale#enabled = 1

let g:ale_linters = {
\   'python': ['flake8', 'mypy', 'pylint', 'pyls'],
\}

"" Enable deoplete
" deoplete.nvim recommend
" set completeopt+=noselect
" let g:deoplete#enable_at_startup = 1
" let g:deoplete#sources#go#gocode_binary = "~/go/bin/gocode"
" let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

"" Python settings
" Add proper PEP8 indentation
" au BufNewFile,BufRead *.py 
"     \ set tabstop=4 |
"     \ set softtabstop=4 |
"     \ set shiftwidth=4 |
"     \ set textwidth=119 |
"     \ set expandtab |
"     \ set autoindent |
"     \ set fileformat=unix

" Auto-format using autopep8
" let g:autopep8_on_save = 1
let g:autopep8_disable_show_diff=1
let g:autopep8_max_line_length=120

" Ignore specific flake8 errors
" let g:flake8_ignore="F405,E501"

"" YouCompleteMe
" let g:ycm_autoclose_preview_window_after_completion=1
" nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
" autocmd CmdwinEnter * inoremap <expr><buffer> <TAB>

"" FZF
"" Run fzf command
nnoremap <Space>p :GitFiles<CR>

"" Search lines in all open vim buffers
function! s:line_handler(l)
  let keys = split(a:l, ':\t')
  exec 'buf' keys[0]
  exec keys[1]
  normal! ^zz
endfunction

function! s:buffer_lines()
  let res = []
  for b in filter(range(1, bufnr('$')), 'buflisted(v:val)')
    call extend(res, map(getbufline(b,0,"$"), 'b . ":\t" . (v:key + 1) . ":\t" . v:val '))
  endfor
  return res
endfunction

command! FZFLines call fzf#run({
\   'source':  <sid>buffer_lines(),
\   'sink':    function('<sid>line_handler'),
\   'options': '--extended --nth=3..',
\   'down':    '60%'
\})

"" Narrow ag results within vim
function! s:ag_to_qf(line)
  let parts = split(a:line, ':')
  return {'filename': parts[0], 'lnum': parts[1], 'col': parts[2],
        \ 'text': join(parts[3:], ':')}
endfunction

function! s:ag_handler(lines)
  if len(a:lines) < 2 | return | endif

  let cmd = get({'ctrl-x': 'split',
               \ 'ctrl-v': 'vertical split',
               \ 'ctrl-t': 'tabe'}, a:lines[0], 'e')
  let list = map(a:lines[1:], 's:ag_to_qf(v:val)')

  let first = list[0]
  execute cmd escape(first.filename, ' %#\')
  execute first.lnum
  execute 'normal!' first.col.'|zz'

  if len(list) > 1
    call setqflist(list)
    copen
    wincmd p
  endif
endfunction

command! -nargs=* Ag call fzf#run({
\ 'source':  printf('ag --nogroup --column --color "%s"',
\                   escape(empty(<q-args>) ? '^(?=.)' : <q-args>, '"\')),
\ 'sink*':    function('<sid>ag_handler'),
\ 'options': '--ansi --expect=ctrl-t,ctrl-v,ctrl-x --delimiter : --nth 4.. '.
\            '--multi --bind=ctrl-a:select-all,ctrl-d:deselect-all '.
\            '--color hl:68,hl+:110',
\ 'down':    '50%'
\ })

"" Call BTags
nnoremap <space>b :BTags <CR>

"" Customizations
"" Replace variable names

" Toggle highlighting
nnoremap <Leader>h :set cursorline!<CR>

" For local replace
nnoremap gr gd[{V%::s/<C-R>///gc<left><left><left>}

" For global replace
nnoremap gR gD:%s/<C-R>///gc<left><left><left>

" Gruvbox customizations
let g:gruvbox_contrast_light="hard"

" add yaml stuffs
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

" neosnippets configuration
let g:go_snippet_engine = "neosnippet"
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

let g:neosnippet#snippets_directory='~/.vim/bundle/vim-go/gosnippets/snippets'
