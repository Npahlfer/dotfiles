set nocompatible " be improved, required

" Plugins
if filereadable(expand("$HOME/.vim/plugins.vim"))
  source $HOME/.vim/plugins.vim
endif

" Settings
if filereadable(expand("$HOME/.vim/settings.vim"))
  source $HOME/.vim/settings.vim
endif

" Set relativenumbers
au BufReadPost,BufNewFile * set relativenumber

" JSON
au BufRead,BufNewFile *.json set ft=json syntax=javascript

if executable('jq')
  autocmd BufRead *.json :%!jq .
endif

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" LESS
au BufNewFile,BufRead *.less set filetype=css

" SCSS
au BufNewFile,BufRead *.scss set filetype=css

autocmd BufNewFile,BufRead *.md set filetype=markdown

" Common Ruby files
au BufRead,BufNewFile Rakefile,Capfile,Gemfile,.autotest,.irbrc,*.treetop,*.tt set ft=ruby syntax=ruby

" Common javascript files
au BufRead,BufNewFile *.jsx,*.ts,*.tsx set ft=javascript syntax=javascript

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

" Speed up transition from modes
if ! has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    " au InsertEnter * set timeoutlen=0
    au InsertEnter * set timeoutlen=100 " Delay to make jk binding work.
    au InsertLeave * set timeoutlen=1000
  augroup END
endif

" Mouse scrolling
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

"clearing highlighted search
nmap <silent> // :nohlsearch<CR>

" Change Working Directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" Status Line
set statusline=%<%f\ %{fugitive#statusline()}\ %h%m%r%=%-14.(%l,%c%V%)\ %{strlen(&fenc)?&fenc:'none'}\ %P
let g:Powerline_symbols = 'fancy'

" Speed up viewport scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Delete buffer without closing window
map <Leader>d :BD<cr>

" easier window navigation.
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

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
nnoremap <Leader>S :%s/\<<C-r><C-w>\>/
xnoremap <leader>S :<c-u>%s/\%V
nnoremap <Leader>s :s/\<<C-r><C-w>\>/
xnoremap <leader>s :<c-u>s/\%V

" select but dont jump
nnoremap <Leader>8 *#
nnoremap <Leader>3 #*

" Unbind u in visual mode
vmap u <NOP>

" Unbind K in normal mode
nmap K <NOP>

" md dot
inoremap ® ⋅

" Remap :W to :w
command! W w
command! Wq wq
command! Wa wa

" Remap :Q to :q
command! Q q

" Bind esc key
inoremap <special> jk <ESC>

" Don't update yank register when deleting single character
" nnoremap x "_x

" Don't update yank register with replaced selection
vnoremap p "_dP
" vnoremap p "_c<Esc>p

noremap "" :registers<CR>

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
map <leader>1 <C-^>
map <Leader>l :bnext<CR>
map <Leader>h :bprev<CR>

" Buffer resizing
nnoremap <Leader>H :vertical resize -4<cr>
nnoremap <Leader>J :resize +5<cr>
nnoremap <Leader>K :resize -5<cr>
nnoremap <Leader>L :vertical resize +5<cr>

" Fix page up and down
map <PageUp> <C-U>
map <PageDown> <C-D>
imap <PageUp> <C-O><C-U>
imap <PageDown> <C-O><C-D>

" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

" Close Quickfix window
map <leader>qq :cclose<CR>

" NERDTree
map <C-s> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Slime
if exists('$TMUX')
  let g:slime_target = "tmux"
  let g:slime_default_config = {"socket_name": split($TMUX, ",")[0], "target_pane": ":.1"}
endif

" Undotree toggle
nnoremap <Leader>u :UndotreeToggle<cr>

" Ultisnips: Trigger configuration.
let g:UltiSnipsSnippetsDir="~/.vim/ultisnips"
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-l>"
let g:UltiSnipsJumpBackwardTrigger="<c-h>"
let g:UltiSnipsListSnippets="<c-0>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" fenced code blocks for markdown
" let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'javascript', 'php']

" vim-javascript, enable flow
let g:javascript_plugin_flow = 1

" vim-pad notes dir
let g:pad#dir = '~/Dropbox/Documents/vimNotes'
let g:pad#maps#list = ["<leader>w", "<C-Esc>"]


let g:LanguageClient_serverCommands = {
    \ 'scss': ['vscode-css-languageserver'],
    \ 'less': ['vscode-css-languageserver'],
    \ 'css': ['vscode-css-languageserver'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ }

nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_rename()<CR>

" let g:python2_host_prog = "~/.pyenv/versions/python2.7"
" let g:python_host_prog = "~/.pyenv/versions/2.7.9/bin/python"
let g:loaded_python_provider = 1

" let g:python_version = matchstr(system("python --version | cut -f2 -d' '"), '^[0-9]')
" if g:python_version =~ 3
"   let g:python2_host_prog = "~/.pyenv/shims/python2.7"
" else
"   let g:python3_host_prog = "~/.pyenv/shims/python3"
" endif

" tern
if exists('g:plugs["tern_for_vim"]')
  " autocmd FileType javascript setlocal omnifunc=tern#Complete
endif

" tern
autocmd FileType javascript nnoremap <silent> <buffer> gb :TernDef<CR>
nnoremap <C-g><C-t> :TernType<CR>

"enable keyboard shortcuts
let g:tern_map_keys=1
"show argument hints
let g:tern_show_argument_hints='on_hold'

" deoplete
let g:deoplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = 1  " This do disable full signature type on autocomplete
let g:deoplete#sources#ternjs#types = 1

"Add extra filetypes
let g:tern#filetypes = [
      \ 'javascript',
      \ 'jsx',
      \ 'tsx',
      \ 'javascript.jsx',
      \ 'vue'
      \ ]

" Use tern_for_vim.
let g:tern#command = ["tern"]
let g:tern#arguments = ["--persistent"]
inoremap <expr><TAB>  pumvisible()? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible()? "\<C-p>" : "\<TAB>"

" inoremap <silent><expr> <TAB>
" \ pumvisible() ? "\<C-n>" :
" \ <SID>check_back_space() ? "\<TAB>" :
" \ deoplete#mappings#manual_complete()
" function! s:check_back_space() abort "{{{
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~ '\s'
" endfunction"}}}

" enable jsx for js files
let g:jsx_ext_required = 0

" Reason format
autocmd FileType reason map <buffer> <Leader>c :ReasonPrettyPrint<Cr>

let g:LanguageClient_serverCommands = {
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ }

nnoremap <silent> gd :call LanguageClient_textDocument_definition()<cr>
nnoremap <silent> gf :call LanguageClient_textDocument_formatting()<cr>
nnoremap <silent> g<cr> :call LanguageClient_textDocument_hover()<cr>

" nnoremap gc :call LanguageClient_contextMenu()<CR>
" Or map each action separately
" nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
" nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_rename()<CR>

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

" let g:ale_echo_cursor = 0
let local_eslint = finddir(getcwd() . '/node_modules') . '/.bin/eslint'
if matchstr(local_eslint, "^\/\\w") == ''
  let local_eslint = local_eslint
endif

let g:ale_javascript_eslint_executable = local_eslint

" " Enable completion where available.
" " let g:ale_completion_enabled = 1
" " let g:ale_completion_delay = 100


" " let g:ale_linter_aliases = {'jsx': 'css'}
let g:ale_sign_error = '?'
let g:ale_sign_warning = '!'


"" Ale config
" filetype off
" let &runtimepath.=',~/.vim/bundle/ale'
let g:ale_sign_column_always = 1
let g:ale_linters_explicit = 1
let g:ale_linters = {
      \   'javascript': ['eslint'],
      \   'jsx': ['eslint'],
      \   'tsx': ['eslint'],
      \   'ts': ['eslint'],
      \}
let g:ale_fixers = {
\   'javascript': ['prettier'],
\   'typescript': ['prettier'],
\   'jsx': ['prettier'],
\}

let g:ale_javascript_prettier_use_global = 0
let g:ale_javascript_prettier_use_local_config = 1
 " --config-precedence prefer-file
let g:ale_javascript_prettier_options = '--single-quote --trailing-comma all --no-semi --bracket-spacing --print-width 80'
let g:ale_fix_on_save = 1
" filetype plugin on

highlight ALEError ctermbg=none cterm=NONE
highlight ALEWarning ctermbg=none cterm=NONE
highlight ALEErrorSign ctermfg=18 ctermbg=73 cterm=bold
highlight ALEWarningSign ctermfg=18 ctermbg=73 cterm=bold

" Ale error shortcuts
nmap <silent> <Leader>k <Plug>(ale_previous_wrap)
nmap <silent> <Leader>j <Plug>(ale_next_wrap)

" nmap <silent> <Leader>w :call ale#definition#GoTo({})<CR>
" nmap <silent> <Leader>r :call ale#references#Find()<CR>

set statusline+=%#warningmsg#
set statusline+=%*

" Editor config
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
let g:EditorConfig_disable_rules = ['trim_trailing_whitespace']

" let g:prettier#exec_cmd_async = 1
" let g:prettier#autoformat = 0
" autocmd BufWritePre *.jsx,*.tsx,*.js,*.css,*.scss,*.less PrettierAsync

" max line lengh that prettier will wrap on
" let g:prettier#config#print_width = 100

" " number of spaces per indentation level
" let g:prettier#config#tab_width = 4

" " use tabs over spaces
" let g:prettier#config#use_tabs = 'false'

" " print semicolons
" let g:prettier#config#semi = 'false'

" " single quotes over double quotes
" let g:prettier#config#single_quote = 'true'

" " print spaces between brackets
" let g:prettier#config#bracket_spacing = 'false'

" " put > on the last line instead of new line
" let g:prettier#config#jsx_bracket_same_line = 'true'

" " none|es5|all
" let g:prettier#config#trailing_comma = 'all'

" " flow|babylon|typescript|postcss
" let g:prettier#config#parser = 'flow'

" Neoformat for Prettier
" autocmd BufWritePost *.js silent Neoformat

" autocmd FileType javascript setlocal formatprg=prettier\ --stdin\ --parser\ flow\ --single-quote\ --trailing-comma\ all\ --no-semi\ --arrow-parens\ always\ --print-width\ 100
" " Use formatprg when available
" let g:neoformat_try_formatprg = 1

" Project specific Prettier
" autocmd FileType javascript setlocal formatprg=prettier\ --stdin\ --parser\ flow\ --single-quote\ --trailing-comma\ es5
" Use formatprg when available
" let g:neoformat_try_formatprg = 1
" let g:neoformat_only_msg_on_error = 1

" nnoremap gp :silent %!prettier --stdin --trailing-comma all --single-quote<CR>

" au VimEnter * RainbowParentheses

" delimitMate
" let delimitMate_balance_matchpairs = 0
" let delimitMate_excluded_ft = "lisp"

" vim-commentary
" Whether the .jsx extension is required.
if !exists('g:jsx_ext_required')
  let g:jsx_ext_required = 1
endif

" Whether the @jsx pragma is required.
if !exists('g:jsx_pragma_required')
  let g:jsx_pragma_required = 0
endif

let s:jsx_pragma_pattern = '\%^\_s*\/\*\*\%(\_.\%(\*\/\)\@!\)*@jsx\_.\{-}\*\/'

" Whether to set the JSX filetype on *.js files.
fu! <SID>EnableJSX()
  if g:jsx_pragma_required && !exists('b:jsx_ext_found')
    " Look for the @jsx pragma.  It must be included in a docblock comment
    " before anything else in the file (except whitespace).
    let b:jsx_pragma_found = search(s:jsx_pragma_pattern, 'npw')
  endif

  if g:jsx_pragma_required && !b:jsx_pragma_found | return 0 | endif
  if g:jsx_ext_required && !exists('b:jsx_ext_found') | return 0 | endif
  return 1
endfu

autocmd BufNewFile,BufRead *.jsx let b:jsx_ext_found = 1
autocmd BufNewFile,BufRead *.jsx set filetype=javascript.jsx
autocmd BufNewFile,BufRead *.js if <SID>EnableJSX() | set filetype=javascript.jsx | endif
" autocmd FileType javascript.jsx setlocal commentstring={/*\ %s\ */}

" let g:context#commentstring#table['javascript.jsx'] = {
" 	  \ 'jsComment' : '// %s',
" 	  \ 'jsImport' : '// %s',
" 	  \ 'jsxStatment' : '// %s',
" 	  \ 'jsxRegion' : '{/*%s*/}',
" 	  \}

" vmap <leader>gg :<c-u>'<,'>s/{\/\*\(.\+\)\*\/}/\1<CR>
" map <leader>gg :s/{\/\*\(.\+\)\*\/}/\1\<cr>

let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme,css,less,scss,javascript,php RainbowParentheses
augroup END

if exists('g:gui_oni')
  au FileType fzf tnoremap <nowait><buffer> <esc> <c-g> "Close FZF in neovim with esc
endif

" fzf
let g:fzf_nvim_statusline = 0 " disable statusline overwriting

let g:fzf_layout = { 'down': '75%' }

command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

nnoremap <silent> <leader><leader> :Files<CR>
nnoremap <silent> <leader>bb :Buffers<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>O :BTags<CR>
nnoremap <silent> <leader>o :Tags<CR>
nnoremap <silent> <leader>? :History<CR>
" nnoremap <silent> <leader>t :Filetypes<CR>

imap <c-x><c-p> <plug>(fzf-complete-path)
imap <C-x><C-f> <plug>(fzf-complete-file-rg)
imap <C-x><C-l> <plug>(fzf-complete-line)

" FZF layout
let g:fzf_layout = { 'down': '75%' }

" The Silver Searcher
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
endif

if executable('rg')
  set grepprg=rg\ --vimgrep\ --color=never\ --iglob\ "!**/dist/**" --iglob "!**/language/**" --iglob "!**/lang/**"'

  " Ripgrep and fzf settings
  command! -bang -nargs=* Rg
	\ call fzf#vim#grep(
	\   'rg --column --line-number --no-heading --iglob "!**/dist/**" --iglob "!**/language/**" --iglob "!**/lang/**" -g "!*.sql" -g "!*.lock" -g "!*.min.js" --color=always '.shellescape(<q-args>), 1,
	\   fzf#vim#with_preview(),
	\   <bang>0)


  nnoremap <leader>/ :Rg<cr>
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
let g:over#command_line#search#enable_move_cursor = 1

" " Yank stack
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste

" gen tags
let g:gen_tags#ctags_auto_gen=1
let g:gen_tags#gtags_auto_gen=1

" Generate js ctags
command! MakeJSTags !find . -type f -iregex ".*\.js$" -not -path "./node_modules/*" -exec jsctags {} -f \; | sed '/^$/d' | sort > tags

" allows for running of script over multiple lines
function! <SID>StripWhitespace ()
  let save_cursor = getpos(".")
  let old_query = getreg('/')
  :%s/\s\+$//e
  call setpos('.', save_cursor)
  call setreg('/', old_query)
endfunction

" auto call function above on save
autocmd BufWritePre * silent if &ft =~ 'sh\|perl\|python\|php\|javascript\|less\|scss\|css\|vim' | :call <SID>StripWhitespace() | endif

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

" macro over multiple lines
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

" Add vim-repeat support to non nativly supported plugins:
" https://github.com/tpope/vim-repeat
" silent! call repeat#set("\<Plug>MyWonderfulPlugin", v:count)

" virtual tabstops using spaces
set shiftwidth=2
set softtabstop=2
set expandtab
" allow toggling between local and default mode
if !exists('*TabToggle')
function TabToggle()
  if &expandtab
    set shiftwidth=2
    set softtabstop=0
    set noexpandtab
  else
    set shiftwidth=2
    set softtabstop=2
    set expandtab
  endif
endfunction
endif
nmap <leader>t mz:execute TabToggle()<CR>'z

" Restore cursor position
autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif

" Ctags generation
command! MakeTags GenCtags
