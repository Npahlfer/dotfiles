set nocompatible " be iMproved, required

" speed up vim
set ttyfast
set lazyredraw

filetype off " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"call vundle#begin('~/some/path')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" -----------------------------------------------------

Plugin 'mattn/emmet-vim'
Plugin 'delimitMate.vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'scrooloose/syntastic'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'othree/html5.vim'
Plugin 'junegunn/vim-easy-align'
Plugin 'captbaritone/better-indent-support-for-php-with-html'
Plugin 'junegunn/fzf.vim'
Plugin 'shawncplus/phpcomplete.vim'
Plugin 'nrocco/vim-phplint'
Plugin 'kshenoy/vim-signature' 
Plugin 'qpkorr/vim-bufkill'
Plugin 'Valloric/YouCompleteMe'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'michaeljsmith/vim-indent-object'

" Plugin 'valloric/MatchTagAlways'
" Plugin 'MarcWeber/vim-addon-mw-utils'
" Plugin 'tomtom/tlib_vim'
" Plugin 'garbas/vim-snipmate'
" Plugin 'honza/vim-snippets' " Default snippets
" Plugin 'mtscout6/syntastic-local-eslint.vim'
" Plugin 'maksimr/vim-jsbeautify'
" Plugin 'junegunn/limelight.vim'

" -----------------------------------------------------

call vundle#end()            " required
filetype plugin indent on    " required

" -----------------------------------------------------

" Set syntax highlighting options.
set t_Co=256
set background=dark
syntax on
colorscheme sidonia "nofrils-dark
hi Normal ctermbg=NONE
hi htmlTag guifg=#00bdec guibg=#200000 gui=bold

" Change mapleader
let mapleader = "\<Space>"

" vim to clipboard
" if $TMUX == ""
"   set clipboard+=unnamed
" endif

set clipboard+=unnamed

" Local dirs
set undofile " save and restore undo history
" set backup
" set backupdir=~/.vim/backups
set directory=~/.vim/swaps
set undodir=~/.vim/undo
set tags^=./tags,tags;/,~/.vim/tags
" set tags=./tags,tags;$HOME
" set tags=tags;/
" set tags+=./tags;/

" Line numbers
set number 

" Incompleted commands are shown
set showcmd 

" Reload files that have been changed outside vim
set autoread 

" Copy indent from last line when starting new line.
set autoindent 

" :let g:html_indent_inctags = "html,body,head,tbody,div,p,span"

" Indents mostly right
:set smartindent   

" Tabs are at proper location
:set tabstop=4     

" Don't use actual tab character (ctrl-v)
:set noexpandtab     

" Indenting is 4 spaces
:set shiftwidth=4  

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

" Allow cursor keys in insert mode.
" set esckeys 

" Column to show folds
" set foldcolumn=4 

set foldenable
set foldlevel=10
" Sets `foldlevel` when editing a new buffer
" set foldlevelstart=10 

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

" Text 
:set textwidth=80

:set colorcolumn=+1

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
set ignorecase 

" Highlight dynamically as pattern is typed.
set incsearch 

" Always show status line
set laststatus=2 

" Enable extended regexes.
set magic 

" Enable mouse in visual modes.
" set mouse=v 

" Set mouse type to xterm.
" set ttymouse=xterm 

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
" set rulerformat=%-14.(%l,%c%V%)\ %P

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

" Send more characters at a given time.
set ttyfast 

" Persistent Undo.
set undofile 

" Use visual bell instead of audible bell
set visualbell 

" Character for CLI expansion (TAB-completion).
set wildignore+=*.jpg,*.jpeg,*.gif,*.png,*.gif,*.psd,*.o,*.obj,*.min.js
set wildignore+=*/bower_components/*,*/node_modules/*
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

" Speed up transition from modes
if ! has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
  augroup END
endif

command! MakeTags !ctags -R .

"clearing highlighted search
nmap <silent> <leader>/ :nohlsearch<CR>

nnoremap <silent> n n:call HLNext(0.4)<cr>
nnoremap <silent> N N:call HLNext(0.4)<cr>

function! HLNext (blinktime)
	set invcursorline
	redraw
	exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
	set invcursorline
	redraw
endfunction

" Change Working Directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" Status Line
hi User1 guibg=#455354 guifg=fg      ctermbg=238 ctermfg=fg  gui=bold,underline cterm=bold,underline term=bold,underline
hi User2 guibg=#455354 guifg=#CC4329 ctermbg=238 ctermfg=196 gui=bold           cterm=bold           term=bold
set statusline=[%n]\ %F\ %2*%h%w%m%r%*%y[%{&ff}→%{strlen(&fenc)?&fenc:'No\ Encoding'}]%=%-16(\ L%l,C%c\ %)%P
let g:Powerline_symbols = 'fancy'

" Speed up viewport scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" % in insert mode
:inoremap <M-5> <Esc>%i

" Set update delay to 250ms
set updatetime=150

" Faster split resizing (+,-)
if bufwinnr(1)
  map + <C-W>+
  map - <C-W>-
endif

" Find, edit, buffers
noremap <Leader>e :edit 

map <Leader>d :BD<cr>

" move faster up/down
nmap <C-j> 5j
xmap <C-j> 5j
nmap <C-k> 5k
xmap <C-k> 5k

" Sudo write (,W)
noremap <leader>W :w !sudo tee %<CR>

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
nnoremap <leader>- :split file<CR>
nnoremap <leader>/ :vsplit file<CR>

noremap <Leader>a =ip

" easy regex replace
:nnoremap <Leader>s :%s/\<<C-r><C-w>\>/

" select but dont jump 
nnoremap <Leader>8 *#

" Unbind u in visual mode, everything to lowercase, no thank you.
vmap u <NOP>

" Unbind K in normal mode
nmap K <NOP>

" no Ex mode
noremap Q <Nop>

" Remap :W to :w
command! W w
command! Wq wq
command! Wa wa

" Expand tabs to spaces
" noremap <Leader-t> :set expandtab 

" Yank from cursor to end of line
nnoremap Y y$

" Toggle show tabs and trailing spaces (,t)
" set lcs=tab:›\ ,trail:·,eol:¬,nbsp:_
" set fcs=fold:-
" nnoremap <silent> <leader>t :set nolist!<CR>

" Clear last search (,qs)
map <silent> <leader>qs <Esc>:noh<CR>
" map <silent> <leader>qs <Esc>:let @/ = ""<CR>

" " Indent/unident block (,]) (,[)
" nnoremap <leader>] >i{<CR>
" nnoremap <leader>[ <i{<CR>

" Buffer navigation (,,) (,]) (,[) (,ls)
map <Leader><Leader> <C-^>
:map <Leader>] :bnext<CR>
:map <Leader>[ :bprev<CR>
map <Leader>ls :buffers<CR>

" Buffer resizing
nnoremap <Leader><left> :vertical resize -4<cr>
nnoremap <Leader><down> :resize +5<cr>
nnoremap <Leader><up> :resize -5<cr>
nnoremap <Leader><right> :vertical resize +5<cr>

" Fix page up and down
map <PageUp> <C-U>
map <PageDown> <C-D>
imap <PageUp> <C-O><C-U>
imap <PageDown> <C-O><C-D>

" Close Quickfix window
map <leader>qq :cclose<CR>

" youCompleteMe preview options
let g:ycm_autoclose_preview_window_after_insertion = 1
"" let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_register_as_syntastic_checker = 0

" enable jsx for js files
let g:jsx_ext_required = 0

" emmet keybindings
autocmd FileType html,css,js,jsx EmmetInstall
let g:user_emmet_install_global = 0
" phplint shortcut
noremap <Leader-l> :Phplint<CR></CR>

if exists(':EasyAlign')
	" Start interactive EasyAlign in visual mode (e.g. vipga)
	xmap ga <Plug>(EasyAlign)
	" Start interactive EasyAlign for a motion/text object (e.g. gaip)
	nmap ga <Plug>(EasyAlign)

	nmap <Leader>= <Plug>(EasyAlign)ip
endif

" Syntastic settings
let local_eslint = finddir('node_modules', '.;') . '/.bin/eslint'
if matchstr(local_eslint, "^\/\\w") == ''
    let local_eslint = getcwd() . "/" . local_eslint
endif
if executable(local_eslint)
    let g:syntastic_javascript_eslint_exec = local_eslint
endif

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 0
" let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exec = 'eslint_d'
" let g:syntastic_javascript_eslint_exe = '$(npm bin)/eslint'
let g:syntastic_css_checkers = ['stylelint']
" let g:syntastic_css_stylelint_exec = 'stylelint_d'

let g:syntastic_aggregate_errors = 0
let g:syntastic_check_on_open=0
let g:syntastic_enable_highlighting = 0
let g:syntastic_echo_current_error=1

let g:syntastic_enable_balloons = 0
let g:syntastic_auto_jump=0

let g:syntastic_full_redraws=1

let g:syntastic_error_symbol = '✖︎'
let g:syntastic_style_error_symbol = '⁉️'
let g:syntastic_warning_symbol = '▲'
let g:syntastic_style_warning_symbol = '△'

highlight link SyntasticErrorSign SignColumn
highlight link SyntasticWarningSign SignColumn
highlight link SyntasticStyleErrorSign SignColumn
highlight link SyntasticStyleWarningSign SignColumn

" Fugitive
if exists(':Gstatus')
	nnoremap <leader>ga :Git add %:p<CR><CR>
	nnoremap <leader>gs :Gstatus<CR>
	nnoremap <leader>gc :Gcommit -a<cr>
	nnoremap <Leader>gb :Gblame<CR>
	nnoremap <leader>gd :Gdiff<CR>
	nnoremap <leader>gl :silent! Glog<CR><bot></bot>copen<CR>
endif

highlight MatchTemp ctermbg=5 ctermfg=3
call matchadd('MatchTemp', 'TEMP', -1)

highlight MatchTodo ctermbg=6 ctermfg=0
call matchadd('MatchTodo', 'TODO', -1)

"Hightligh columns that are over 80.
highlight ColorColumn ctermbg=0 ctermfg=231

" Insert newline
map <leader><Enter> o<ESC>

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
endif

if executable('rg')
  set grepprg=rg\ --color=never
endif

" fzf
if exists(':Fzf')
	set rtp+=/usr/local/opt/fzf
	" map <leader><tab> :FZF -x<cr>
	map <leader><tab> :Files<cr>

	" FZF layout
	let g:fzf_layout = { 'up': '95%' }

	command! -bang -nargs=* Rg
	  \ call fzf#vim#grep(
	  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
	  \   <bang>0 ? fzf#vim#with_preview('up:60%')
	  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
	  \   <bang>0)

	command! -bang -nargs=? -complete=dir Files
				\ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

	" Mapping selecting mappings
	" nmap <leader><tab> <plug>(fzf-maps-n)
	" xmap <leader><tab> <plug>(fzf-maps-x)
	" omap <leader><tab> <plug>(fzf-maps-o)

	" Insert mode completion
	imap <c-x><c-p> <plug>(fzf-complete-path)
	imap <c-x><c-f> <plug>(fzf-complete-file-ag)
	imap <c-x><c-l> <plug>(fzf-complete-line)

	" imap <c-x><c-k> <plug>(fzf-complete-word)
	" inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})

	" Replace the default dictionary completion with fzf-based fuzzy completion
	" inoremap <expr> <c-x><c-k> fzf#complete('cat /usr/share/dict/words')

	" noremap <Leader>c :Commits<cr>

	" Fuzzy search changed files (git ls-files)
	command! Fzfc call fzf#run(fzf#wrap(
				\ {'source': 'git ls-files --exclude-standard --others --modified'}))
	noremap <Leader>l :Fzfc<cr>

	noremap <Leader>m :Marks<cr>

	" Fuzzy search buffers
	noremap <Leader>b :Buffers<cr>
	" map <silent> <Leader>b :call fzf#run(fzf#wrap(
	"     \ {'source': map(range(1, bufnr('$')), 'bufname(v:val)')}))<CR>

	noremap <Leader>t :Tags<cr>

endif

" quicklook
map <Leader>v :write<cr>:sil !/usr/bin/qlmanage -p % > /dev/null &<cr>:redraw!<cr>

" Quick search n replace
nnoremap <silent> <Leader>* :let @/='\<<C-R>=expand("<cword>")<CR>\>'<CR>:set hls<CR>
vnoremap <silent> <Leader>* :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy:let @/=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>:set hls<CR>


" Search and replace word under cursor (,*)
nnoremap <leader>* :%s/\<<C-r><C-w>\>//<Left>

" Search for the word under cursor in current dir and sub dirs
nnoremap <leader>q q:ivimgrep<Space>//<Space>**/*[ch]<Bar>copen<Esc>F/;innoremap <leader>g :lvimgrep /<C-r><C-w>/j **/* expand("%:e") \|lopen
nnoremap <leader>f :Rg<cr>

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
autocmd BufWritePre * if &ft =~ 'sh\|perl\|python\|php\|javascript\|less\|css' | :call <SID>StripWhitespace() | endif

" Auto complete search.
" set wildchar=<Tab> wildmenu wildmode=full

" Toggle Vexplore with Ctrl-E
function! ToggleVExplorer()
    if exists("t:expl_buf_num")
        let expl_win_num = bufwinnr(t:expl_buf_num)
        if expl_win_num != -1
            let cur_win_nr = winnr()
            exec expl_win_num . 'wincmd w'
            close
            exec cur_win_nr . 'wincmd w'
            unlet t:expl_buf_num
        else
            unlet t:expl_buf_num
        endif
    else
        exec '1wincmd w'
        Vexplore
        let t:expl_buf_num = bufnr("%")
    endif
endfunction
map <silent> <C-E> :call ToggleVExplorer()<CR>

" Default to tree mode
let g:netrw_liststyle = 3

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

" Enables CSS auto-completion
set omnifunc=csscomplete#CompleteCSS

" Format as json, regardless of the fileending
command! -range -nargs=0 -bar JsonTool <line1>,<line2>!python -m json.tool

" JSON
au BufRead,BufNewFile *.json set ft=json syntax=javascript

" Common Ruby files
au BufRead,BufNewFile Rakefile,Capfile,Gemfile,.autotest,.irbrc,*.treetop,*.tt set ft=ruby syntax=ruby

" Common javascript files
au BufRead,BufNewFile *.jsx,*.ts set ft=javascript syntax=javascript

" Coffee Folding
au BufNewFile,BufReadPost *.coffee setl foldmethod=indent nofoldenable

" ZSH
au BufRead,BufNewFile .zsh_rc,.functions,.commonrc set ft=zsh

