if [ $# -eq 0 ]; then
  cd $DOTFILES_LOCATION
else
  make -C $DOTFILES_LOCATION $@
fi
