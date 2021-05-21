" Install vim-plug if not found
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Interests:
" - https://github.com/folke/which-key.nvim
" - Theme: https://github.com/shaunsingh/nord.nvim
" - or...: https://www.reddit.com/r/neovim/comments/n80ah6/moonlightnvim_lua_port_of_moonlight_vscode_for/
" - https://www.reddit.com/r/neovim/comments/neakgd/zen_mode_a_new_distractionfree_coding_plugin/
" - https://github.com/nvim-telescope/telescope.nvim
" - https://www.reddit.com/r/neovim/comments/n8kxx7/emacs_to_neovim/
call plug#begin('~/.config/nvim/plugged')

  Plug 'junegunn/fzf'

call plug#end()

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif
