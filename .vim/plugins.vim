filetype off

function! InstallPlugins ()
  :source ~/.vimrc
  :PlugInstall
endfunction

autocmd! BufWritePost vundle.vim :call InstallPlugins()

call plug#begin('~/.vim/bundle')

" -----------------------------------------------------

Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
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
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'maxbrunsfeld/vim-yankstack'
" Plug 'tpope/vim-markdown'
Plug 'suan/vim-instant-markdown'
Plug 'fmoralesc/vim-pad'
Plug 'editorconfig/editorconfig-vim'

" ---- Archive ----------------------------------------
" Plug 'Valloric/YouCompleteMe'
" Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'tomtom/tlib_vim'
" Plug 'garbas/vim-snipmate'
" Plug 'honza/vim-snippets' " Default snippets
" Plug 'maksimr/vim-jsbeautify'
" Plug 'neomake/neomake'

" -----------------------------------------------------

call plug#end()

filetype plugin indent on
