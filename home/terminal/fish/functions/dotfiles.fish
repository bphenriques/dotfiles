if test (count $argv) -eq 0
  cd $DOTFILES_LOCATION
else
  make -C $DOTFILES_LOCATION $argv
end
