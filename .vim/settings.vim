" Set syntax highlighting options.
set t_Co=256
set background=dark
syntax on
colorscheme sidonia 
hi Normal ctermbg=NONE
hi htmlTag guifg=#00bdec guibg=#200000 gui=bold

" Change colour of tildes at the end of the buffer.
highlight EndOfBuffer ctermfg=black

" Border colour
hi! VertSplit ctermfg=2 ctermbg=NONE term=NONE

highlight MatchTemp ctermbg=5 ctermfg=3
call matchadd('MatchTemp', 'TEMP', -1)

highlight MatchTodo ctermbg=3 ctermfg=5
call matchadd('MatchTodo', 'TODO', -1)

" "Hightligh columns that are over 80.
highlight colorcolumn ctermbg=0 ctermfg=1

" Hightlight parens colour.
highlight! MatchParen ctermfg=16 ctermbg=93 cterm=bold

" speed up vim
set ttyfast
set lazyredraw

" Change mapleader
let mapleader = "\<Space>"

set clipboard+=unnamed

" Local dirs
set undofile " save and restore undo history
set directory=~/.vim/swaps
set undodir=~/.vim/undo
set tags^=./tags,tags;/,~/.vim/tags

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

set foldenable
set foldlevel=10

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
set wildignore+=*/bower_components/*,*/node_modules/*,node_modules/*
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
set relativenumber

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

" Set update delay to 150ms
set updatetime=150

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

