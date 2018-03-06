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
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" LESS
au BufNewFile,BufRead *.less set filetype=css

autocmd BufNewFile,BufRead *.md set filetype=markdown

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

" Speed up transition from modes
if ! has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
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
nnoremap <Leader>3 #*

" Unbind u in visual mode
vmap u <NOP>

" Unbind K in normal mode
nmap K <NOP>

" no Ex mode
" noremap Q <Nop>

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

" Undotree toggle
nnoremap <Leader>u :UndotreeToggle<cr>

" fenced code blocks for markdown
" let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'javascript', 'php']

" vim-javascript, enable flow
let g:javascript_plugin_flow = 1

" vim-pad notes dir
let g:pad#dir = '~/Dropbox/Documents/vimNotes'
let g:pad#maps#list = ["<leader>w", "<C-Esc>"]

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

  inoremap <silent><expr> <TAB>
	\ pumvisible() ? "\<C-n>" :
	\ <SID>check_back_space() ? "\<TAB>" :
	\ deoplete#mappings#manual_complete()
  function! s:check_back_space() abort "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction"}}}
endif

" enable jsx for js files
let g:jsx_ext_required = 0

" Reason format
autocmd FileType reason map <buffer> <Leader>c :ReasonPrettyPrint<Cr>

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

" Enable completion where available.
let g:ale_completion_enabled = 1"

let g:ale_fixers = {
      \   'javascript': ['eslint']
      \}

let g:ale_linters = {
      \   'javascript': ['eslint'],
      \   'jsx': ['eslint'],
      \}

let g:ale_linter_aliases = {'jsx': 'css'}

let g:ale_sign_error = '?'
let g:ale_sign_warning = '!'

highlight ALEErrorSign ctermfg=18 ctermbg=73 cterm=bold
highlight ALEWarningSign ctermfg=18 ctermbg=73 cterm=bold

" Ale error shortcuts
nmap <silent> <Leader>k <Plug>(ale_previous_wrap)
nmap <silent> <Leader>j <Plug>(ale_next_wrap)

set statusline+=%#warningmsg#
set statusline+=%*

" Editor config
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
let g:EditorConfig_disable_rules = ['trim_trailing_whitespace']

" Neoformat for Prettier
" autocmd BufWritePost *.js silent Neoformat

" Project specific Prettier
" autocmd FileType javascript setlocal formatprg=prettier\ --stdin\ --parser\ flow\ --single-quote\ --trailing-comma\ es5
" Use formatprg when available
" let g:neoformat_try_formatprg = 1
" let g:neoformat_only_msg_on_error = 1

" nnoremap gp :silent %!prettier --stdin --trailing-comma all --single-quote<CR>

" au VimEnter * RainbowParentheses

let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

" Elm keybindings
let g:elm_setup_keybindings = 0

let g:elm_format_autosave = 1

" fzf
let g:fzf_nvim_statusline = 0 " disable statusline overwriting

let g:fzf_layout = { 'down': '75%' }

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
let g:fzf_layout = { 'down': '75%' }

" The Silver Searcher
if executable('ag')
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
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste

" gen tags
" let g:gen_tags#ctags_auto_gen=1
" let g:gen_tags#gtags_auto_gen=0

" Generate js ctags
command! MakeJSTags !find . -type f -iregex ".*\.js$" -not -path "./node_modules/*" -exec jsctags {} -f \; | sed '/^$/d' | sort > tags

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
noremap <leader>S :call <SID>StripWhitespace ()<CR>

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
"
" Syntax highlighting group for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" Format as json, regardless of the fileending
command! -range -nargs=0 -bar JsonTool <line1>,<line2>!python -m json.tool

" Copy search matches
function! CopyMatches(reg)
  let hits = []
  %s//\=len(add(hits, submatch(0))) ? submatch(0) : ''/gne
  let reg = empty(a:reg) ? '+' : a:reg
  execute 'let @'.reg.' = join(hits, "\n") . "\n"'
endfunction
command! -register CopyMatches call CopyMatches(<q-reg>)

