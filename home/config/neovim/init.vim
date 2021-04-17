if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/fzf'

call plug#end()

" Install missing plugins
autocmd! VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)')) | PlugInstall --sync | q | endif
