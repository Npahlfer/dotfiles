filetype off

function! InstallPlugins ()
  :source ~/.vimrc
  :PlugInstall
endfunction

autocmd! BufWritePost vundle.vim :call InstallPlugins()

call plug#begin('~/.vim/bundle')

" -----------------------------------------------------

" Plug 'Raimondi/delimitMate'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'SirVer/ultisnips'
Plug 'captbaritone/better-indent-support-for-php-with-html'
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'editorconfig/editorconfig-vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'honza/vim-snippets'
Plug 'idanarye/vim-yankitute'
Plug 'jsfaint/gen_tags.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/gv.vim'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'junegunn/vim-easy-align'
Plug 'kshenoy/vim-signature'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'mbbill/undotree'
Plug 'michaeljsmith/vim-indent-object'
" Plug 'mxw/vim-jsx'
Plug 'suy/vim-context-commentstring'
Plug 'scrooloose/nerdtree'
Plug 'nrocco/vim-phplint'
Plug 'osyo-manga/vim-over'
" Plug 'justinmk/vim-sneak'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'qpkorr/vim-bufkill'
Plug 'reasonml-editor/vim-reason-plus'
Plug 'sbdchd/neoformat'
Plug 'shawncplus/phpcomplete.vim'
Plug 'sheerun/vim-polyglot'
Plug 'ternjs/tern_for_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
" Plug 'vim-scripts/paredit.vim'
Plug 'jpalardy/vim-slime'
Plug 'tbodt/deoplete-tabnine', { 'do': './install.sh' }
" Plug 'flowtype/vim-flow', {
"       \ 'autoload': {
"       \     'filetypes': 'javascript'
"       \ }}
Plug 'prettier/vim-prettier', {
    \ 'do': 'npm install',
    \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss'] }
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

" ---- Archive ----------------------------------------
" Plug 'zxqfl/tabnine-vim'
" Plug 'sbdchd/neoformat'
" Plug 'reasonml-editor/vim-reason'
" Plug 'tpope/vim-markdown'
" Plug 'suan/vim-instant-markdown'
" Plug 'fmoralesc/vim-pad'
" Plug 'christoomey/vim-tmux-navigator'
" Plug 'Valloric/YouCompleteMe'
" Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'tomtom/tlib_vim'
" Plug 'garbas/vim-snipmate'
" Plug 'maksimr/vim-jsbeautify'
" Plug 'neomake/neomake'

" -----------------------------------------------------

call plug#end()

filetype plugin indent on
