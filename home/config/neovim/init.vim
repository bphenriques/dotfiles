"
" Plugins - Install vim-plug if not installed
"
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"
" Plugins - Declaration
"
" Interests:
" - https://www.reddit.com/r/neovim/comments/neakgd/zen_mode_a_new_distractionfree_coding_plugin/
" - https://github.com/nvim-telescope/telescope.nvim
" - https://www.reddit.com/r/neovim/comments/n8kxx7/emacs_to_neovim/
call plug#begin('~/.config/nvim/plugged')
  Plug 'junegunn/fzf'         " Fuzzy finding
  Plug 'folke/which-key.nvim' " Makes it easier to learn shortcuts
  Plug 'shaunsingh/nord.nvim' " Theme
call plug#end()

"
" Plugins - Run PlugInstall for missing plugins
"
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

"
" Theming
"
let g:nord_contrast = 1
let g:nord_borders = 1
let g:nord_disable_background = 1
colorscheme nord

"
" Configuration
"
