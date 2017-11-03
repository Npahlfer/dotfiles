set nocompatible " be improved, required

" speed up vim
set ttyfast
set lazyredraw

filetype off " required

call plug#begin('~/.vim/bundle')

" -----------------------------------------------------

Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
" Plug 'scrooloose/syntastic'
Plug 'w0rp/ale'
Plug 'hail2u/vim-css3-syntax'
Plug 'othree/html5.vim'
Plug 'junegunn/vim-easy-align'
Plug 'captbaritone/better-indent-support-for-php-with-html'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'shawncplus/phpcomplete.vim'
Plug 'nrocco/vim-phplint'
Plug 'kshenoy/vim-signature' 
Plug 'qpkorr/vim-bufkill'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'michaeljsmith/vim-indent-object'
Plug 'elmcast/elm-vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'osyo-manga/vim-over'
Plug 'ternjs/tern_for_vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'sbdchd/neoformat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-eunuch'
Plug 'mbbill/undotree'
Plug 'jsfaint/gen_tags.vim'
Plug 'reasonml-editor/vim-reason'
" Plug 'neomake/neomake'
" Plug 'maxbrunsfeld/vim-yankstack'
" Plug 'Galooshi/vim-import-js'

" ---- Plugins archive --------------------------------
" Plug 'Valloric/YouCompleteMe'
" Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'tomtom/tlib_vim'
" Plug 'garbas/vim-snipmate'
" Plug 'honza/vim-snippets' " Default snippets
" Plug 'maksimr/vim-jsbeautify'

" -----------------------------------------------------

call plug#end()
filetype plugin indent on  " required

" -----------------------------------------------------

" Set syntax highlighting options.
set t_Co=256
set background=dark
syntax on
colorscheme sidonia 
hi Normal ctermbg=NONE
hi htmlTag guifg=#00bdec guibg=#200000 gui=bold

" Change mapleader
let mapleader = "\<Space>"

set clipboard+=unnamed

" Local dirs
set undofile " save and restore undo history
set directory=~/.vim/swaps
set undodir=~/.vim/undo
set tags^=./tags,tags;/,~/.vim/tags
" set backup
" set backupdir=~/.vim/backups

" Line numbers
set number 

" Incompleted commands are shown
set showcmd 

" Reload files that have been changed outside vim
set autoread 

" Stricter rules for C programs
:set cindent       

set backspace=indent,eol,start

" Highlight current line
set cursorline 

" Add vertical spaces to keep right and left aligned
set diffopt=filler 

" Ignore whitespace changes (focus on code changes)
set diffopt+=iwhite 

" BOM often causes trouble
set encoding=utf-8 nobomb 

" Border char
set fillchars+=vert:\ 

" Change colour of tildes at the end of the buffer.
highlight EndOfBuffer ctermfg=black

" Border colour
hi! VertSplit ctermfg=2 ctermbg=NONE term=NONE

set foldenable
set foldlevel=10
" Sets `foldlevel` when editing a new buffer
" set foldlevelstart=10 

" Column to show folds
" set foldcolumn=4 

" Markers are used to specify folds.
set foldmethod=syntax 

" Allow folding single lines
set foldminlines=0 

" Set max fold nesting level
set foldnestmax=3 

" Format comments
set formatoptions+=c 

" Format comments with gq
set formatoptions+=q 

" Recognize numbered lists
set formatoptions+=n 

" Use indent from 2nd line of a paragraph
set formatoptions+=2 

" Don't break lines that are already long
set formatoptions+=l 

" Break before 1-letter words
set formatoptions+=1 

" By default add g flag to search/replace. Add g to toggle.
set gdefault 

" When a buffer is brought to foreground, remember undo history and marks.
set hidden 

" Remember copy history after quiting.
set viminfo='20,\"500

" Increase history from 20 default to 1000
set history=1000 

" Highlight searches
set hlsearch 

" Ignore case of searches.
" set ignorecase 

" Highlight dynamically as pattern is typed.
set incsearch 

" Always show status line
set laststatus=2 

" Enable extended regexes.
set magic 

" Disable error bells.
set noerrorbells 

" Only insert single space after a '.', '?' and '!' with a join command.
set nojoinspaces 

" Don't reset cursor to start of line when moving around.
set nostartofline 

" set nowrap " Do not wrap lines.
set wrap

" Set omni-completion method.
set ofu=syntaxcomplete#Complete 

" Show all changes.
set report=0 

" Show the cursor position
set ruler 

" Start scrolling three lines before horizontal border of window.
set scrolloff=3 

" Start scrolling three columns before vertical border of window.
set sidescrolloff=3 

" Don't show the intro message when starting vim.
set shortmess=atI 

" Show the current mode.
set showmode 

" Show tab bar if there is more than one.
set showtabline=1 

" Ignore 'ignorecase' if search patter contains uppercase characters.
set smartcase 

" At start of line, <Tab> inserts shiftwidth spaces, <Bs> deletes shiftwidth spaces.
set smarttab 

" Tab key results in 2 spaces
" set softtabstop=4 

" New window goes below
set splitbelow 

" New windows goes right
set splitright 

set suffixes=.bak,~,.swp,.swo,.o,.d,.info,.aux,.log,.dvi,.pdf,.bin,.bbl,.blg,.brf,.cb,.dmg,.exe,.ind,.idx,.ilg,.inx,.out,.toc,.pyc,.pyd,.dll

" Show the filename in the window titlebar.
set title 

" Use visual bell instead of audible bell
set visualbell 

" Character for CLI expansion (TAB-completion).
set wildignore+=*.jpg,*.jpeg,*.gif,*.png,*.gif,*.psd,*.o,*.obj,*.min.js,*.sql
set wildignore+=*/bower_components/*,*/node_modules/*
set wildignore+=*/wp-/plugins/*,*/w3tc-config/*,*/cache/*
set wildignore+=*/smarty/*,*/vendor/*,*/.git/*,*/.hg/*,*/.svn/*,*/.sass-cache/*,*/log/*,*/tmp/*,*/build/*,*/ckeditor/*,*/doc/*,*/source_maps/*,*/dist/*

" Hitting TAB in command mode will show possible completions above command line.
set wildmenu 

" Complete only until point of ambiguity.
set wildmode=list:longest 

"Allow splits to be reduced to a single line.
set winminheight=0 

" Searches wrap around end of file
set wrapscan 

" match to be used with %
set matchpairs+=<:> 

" No extra spaces between rows
set linespace=0 

set scrolljump=5
set scrolloff=3

" Set relative line numbers
set relativenumber " Use relative line numbers. Current line is still in status bar.
au BufReadPost,BufNewFile * set relativenumber

" Copy indent from last line when starting new line.
set autoindent 

" Indents mostly right
set smartindent   

" Tabs are at proper location
set tabstop=4     

" Don't use actual tab character (ctrl-v)
set noexpandtab     

" Indenting is 4 spaces
set shiftwidth=4  

" Text columns
set colorcolumn=80
" set textwidth=80

" mouse on
set mouse=a
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

" Set update delay to 150ms
set updatetime=150

" JSON
au BufRead,BufNewFile *.json set ft=json syntax=javascript

if executable('jq')
	autocmd BufRead *.json :%!jq .
endif

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" LESS
au BufNewFile,BufRead *.less set filetype=css

" Common Ruby files
au BufRead,BufNewFile Rakefile,Capfile,Gemfile,.autotest,.irbrc,*.treetop,*.tt set ft=ruby syntax=ruby

" Common javascript files
au BufRead,BufNewFile *.jsx,*.ts set ft=javascript syntax=javascript

" Coffee Folding
au BufNewFile,BufReadPost *.coffee setl foldmethod=indent nofoldenable

" ZSH
au BufRead,BufNewFile .zsh_rc,.functions,.commonrc set ft=zsh

" Python
au FileType python setl sw=2 sts=2 et

" Haskell
au FileType haskell setl sw=4 sts=4 et tabstop=8 shiftround

" Cabal
au FileType cabal setl sw=4 sts=4 et tabstop=8 shiftround

highlight MatchTemp ctermbg=5 ctermfg=3
call matchadd('MatchTemp', 'TEMP', -1)

highlight MatchTodo ctermbg=3 ctermfg=5
call matchadd('MatchTodo', 'TODO', -1)

" "Hightligh columns that are over 80.
highlight colorcolumn ctermbg=0 ctermfg=1

" Speed up transition from modes
if ! has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
  augroup END
endif

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

"clearing highlighted search
nmap <silent> <leader>/ :nohlsearch<CR>

" Change Working Directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" Status Line
set statusline=%<%f\ %{fugitive#statusline()}\ %h%m%r%=%-14.(%l,%c%V%)\ %{strlen(&fenc)?&fenc:'none'}\ %P
hi StatusLine ctermbg=0 ctermfg=7
hi StatusLineNC ctermbg=0 ctermfg=2
let g:Powerline_symbols = 'fancy'

" autocomplete window colours.
highlight Pmenu ctermfg=15 ctermbg=239
highlight PmenuSel ctermfg=250 ctermbg=236

" Speed up viewport scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Delete buffer without closing window
map <Leader>d :BD<cr>

" move faster up/down
nmap <C-j> 5j
xmap <C-j> 5j
nmap <C-k> 5k
xmap <C-k> 5k

" copy line and leave a marker
nnoremap yy yymy

" Disable arrow keys.
inoremap <Up>    <NOP>
nnoremap <Up>    <NOP>
inoremap <Down>  <NOP>
nnoremap <Down>  <NOP>
inoremap <Left>  <NOP>
nnoremap <Left>  <NOP>
inoremap <Right> <NOP>
nnoremap <Right> <NOP>

" split current window
nnoremap <C-w>- :split file<CR>
nnoremap <C-w>\ :vsplit file<CR>
nnoremap <leader>- :split file<CR>
nnoremap <leader>\ :vsplit file<CR>

" Auto align inner paragraph
noremap <Leader>a =ip

" easy regex replace for current word
nnoremap <Leader>s :%s/\<<C-r><C-w>\>/
xnoremap <leader>s :<c-u>%s/\%V

" select but dont jump 
nnoremap <Leader>8 *#

" Unbind u in visual mode
vmap u <NOP>

" Unbind K in normal mode
nmap K <NOP>

" no Ex mode
noremap Q <Nop>

" Remap :W to :w
command! W w
command! Wq wq
command! Wa wa

" Remap :Q to :q
command! Q q

" Toggle show tabs and trailing spaces
set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_,space:-
set fcs=fold:-
nnoremap <silent> <leader>T :set nolist!<CR>

" Clear last search
map <silent> <leader>qs <Esc>:noh<CR>
nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>

" Indent/unident block
nnoremap <leader>] >i[
nnoremap <leader>[ <i[
nnoremap <leader>} >i{
nnoremap <leader>{ <i{

" Buffer navigation
map <Tab><Tab> <C-^>
map <Leader>l :bnext<CR>
map <Leader>h :bprev<CR>

" Buffer resizing
nnoremap <Leader>H :vertical resize -4<cr>
nnoremap <Leader>J :resize +5<cr>
nnoremap <Leader>K :resize -5<cr>
nnoremap <Leader>L :vertical resize +5<cr>

" Center and maximize current buffer
" nnoremap <Leader>c <C-w>_<C-w>|

" Fix page up and down
map <PageUp> <C-U>
map <PageDown> <C-D>
imap <PageUp> <C-O><C-U>
imap <PageDown> <C-O><C-D>

" Close Quickfix window
map <leader>qq :cclose<CR>

" Undotree toggle
nnoremap <Leader>u :UndotreeToggle<cr>

" vim-javascript, enable flow
let g:javascript_plugin_flow = 1

" tern
if exists('g:plugs["tern_for_vim"]')
  let g:tern_show_argument_hints = 'on_hold'
  let g:tern_show_signature_in_pum = 1
  autocmd FileType javascript setlocal omnifunc=tern#Complete
endif

" tern 
autocmd FileType javascript nnoremap <silent> <buffer> gb :TernDef<CR>
nnoremap <C-g><C-t> :TernType<CR>

if executable('nvim')
	" Disable youCompleteMe	
	let g:loaded_youcompleteme = 1

	" deoplete
	
	let g:deoplete#enable_at_startup = 1
	let g:neocomplete#enable_smart_case = 1
	let g:tern_request_timeout = 1
	let g:tern_show_signature_in_pum = 1  " This do disable full signature type on autocomplete
	let g:deoplete#sources#ternjs#types = 1

	"Add extra filetypes
	let g:tern#filetypes = [
	      \ 'jsx',
	      \ 'javascript.jsx',
	      \ 'vue'
	      \ ]
	
	" Use tern_for_vim.
	let g:tern#command = ["tern"]
	let g:tern#arguments = ["--persistent"]
	
	" deoplete tab-complete
	" inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

	inoremap <silent><expr> <TAB>
	      \ pumvisible() ? "\<C-n>" :
	      \ <SID>check_back_space() ? "\<TAB>" :
	      \ deoplete#mappings#manual_complete()
	function! s:check_back_space() abort "{{{
		let col = col('.') - 1
		return !col || getline('.')[col - 1]  =~ '\s'
	endfunction"}}}

	" Autoclose the popup window
	" autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
else 
	" youCompleteMe preview options
	let g:ycm_autoclose_preview_window_after_insertion = 1
	" let g:ycm_autoclose_preview_window_after_completion = 1
	" let g:ycm_register_as_syntastic_checker = 0

	let g:ycm_semantic_triggers = {
	      \ 'elm' : ['.'],
	      \}
endif

" enable jsx for js files
let g:jsx_ext_required = 0

" Reason format
autocmd FileType reason map <buffer> <M-M> :ReasonPrettyPrint<Cr>

" phplint shortcut
noremap <Leader-l> :Phplint<CR></CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

nmap <Leader>= <Plug>(EasyAlign)ip
nmap <Leader>: <Plug>(EasyAlign)ip<Right>:
xmap ga: <Plug>(EasyAlign)<Right>:
nmap ga: <Plug>(EasyAlign)<Right>:

" Ale

let local_eslint = finddir(getcwd() . '/node_modules') . '/.bin/eslint'
if matchstr(local_eslint, "^\/\\w") == ''
    let local_eslint = local_eslint
endif

let g:ale_javascript_eslint_executable = local_eslint

" Enable completion where available.
let g:ale_completion_enabled = 1"

let g:ale_fixers = {
\   'javascript': ['eslint']
\}

" let g:ale_linters = {'jsx': ['stylelint', 'eslint'], 'javascript': ['stylelint', local_eslint]}
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'jsx': ['eslint'],
\}

let g:ale_linter_aliases = {'jsx': 'css'}
" let g:ale_set_highlights = 0

let g:ale_sign_error = '?!'
let g:ale_sign_warning = '!'

highlight ALEErrorSign ctermfg=18 ctermbg=73 cterm=bold
highlight ALEWarningSign ctermfg=18 ctermbg=73 cterm=bold

" Ale error shortcuts
nmap <silent> <Leader>k <Plug>(ale_previous_wrap)
nmap <silent> <Leader>j <Plug>(ale_next_wrap)

set statusline+=%#warningmsg#
set statusline+=%*

" Neoformat for Prettier
" autocmd BufWritePost *.js silent Neoformat

" Project specific Prettier
" autocmd FileType javascript setlocal formatprg=prettier\ --stdin\ --parser\ flow\ --single-quote\ --trailing-comma\ es5
" Use formatprg when available
" let g:neoformat_try_formatprg = 1
" let g:neoformat_only_msg_on_error = 1

" nnoremap gp :silent %!prettier --stdin --trailing-comma all --single-quote<CR>

" js import
" silent! nnoremap <buffer> <unique> <silent> <Leader>g :ImportJSGoto<CR>

au VimEnter * RainbowParentheses

let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

" Elm keybindings
let g:elm_setup_keybindings = 0

let g:elm_format_autosave = 1

" fzf
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting

  let g:fzf_layout = { 'down': '65%' }

  command! -bang -nargs=? -complete=dir Files
	\ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

  nnoremap <silent> <leader><tab> :Files<CR>
  nnoremap <silent> <leader><leader> :Buffers<CR>
  nnoremap <silent> <leader>; :BLines<CR>
  nnoremap <silent> <leader>O :BTags<CR>
  nnoremap <silent> <leader>o :Tags<CR>
  nnoremap <silent> <leader>? :History<CR>

  nnoremap <silent> <leader>ft :Filetypes<CR>

  imap <c-x><c-p> <plug>(fzf-complete-path)
  imap <C-x><C-f> <plug>(fzf-complete-file-rg)
  imap <C-x><C-l> <plug>(fzf-complete-line)

" FZF layout
let g:fzf_layout = { 'down': '45%' }

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
endif

if executable('rg')
	set grepprg=rg\ --vimgrep\ --color=never\ --glob\ "!*/plugins/*"'

	" Ripgrep and fzf settings
	command! -bang -nargs=* Rg
	      \ call fzf#vim#grep(
	      \   'rg --column --line-number --no-heading --glob "!*/dist/*" --glob "!*/plugins/*" -g "!*.sql" -g "!*.min.js" --color=always '.shellescape(<q-args>), 1,
	      \   <bang>0 ? fzf#vim#with_preview('up:60%')
	      \           : fzf#vim#with_preview('right:50%:hidden', '?'),
	      \   <bang>0)
endif

" Fugitive
nnoremap <silent> <leader>ga :Git add %:p<CR><CR>
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gc :Gcommit -a<cr>
nnoremap <silent> <Leader>gb :Gblame<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gl :GV<CR>

" vim over keybindings
nnoremap <leader>F :<c-u>OverCommandLine<cr>%s/\<<C-r><C-w>\>/<C-r><C-w>
nnoremap <leader>f :<c-u>OverCommandLine<cr>%s/
xnoremap <leader>f :<c-u>OverCommandLine<cr>%s/\%V
nnoremap g? :<c-u>OverCommandLine<cr>?
nnoremap g/ :<c-u>OverCommandLine<cr>/
nnoremap g/r :<c-u>OverCommandLine<cr>%s/
xnoremap g/r :<c-u>OverCommandLine<cr>%s/\%V
let g:over_command_line_prompt = ": "

" " Yank stack
" nmap <leader>p <Plug>yankstack_substitute_older_paste
" nmap <leader>P <Plug>yankstack_substitute_newer_paste

" gen tags
let g:gen_tags#ctags_auto_gen=1
let g:gen_tags#gtags_auto_gen=0

" Search files with fzf and ripgrep
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
nnoremap <leader>/ :Rg<cr>

" allows for running of script over multiple lines
function! <SID>StripWhitespace ()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    :%s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfunction
noremap <leader>ss :call <SID>StripWhitespace ()<CR>

" auto call function above on save
autocmd BufWritePre * silent if &ft =~ 'sh\|perl\|python\|php\|javascript\|less\|css' | :call <SID>StripWhitespace() | endif

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

" macro over multiple lines
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

" Add vim-repeat support to non nativly supported plugins:
" https://github.com/tpope/vim-repeat
" silent! call repeat#set("\<Plug>MyWonderfulPlugin", v:count)

" Restore cursor position
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif


" Ctags generation
command! MakeTags GenCtags

" Format as json, regardless of the fileending
command! -range -nargs=0 -bar JsonTool <line1>,<line2>!python -m json.tool

" Hightlight parens colour.
highlight! MatchParen ctermfg=16 ctermbg=93 cterm=bold

