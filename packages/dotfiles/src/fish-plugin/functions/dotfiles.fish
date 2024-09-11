function dotfiles -d 'Interactive dotfiles'
  if test (count $argv) -eq 0
    cd $DOTFILES_LOCATION
  else
    command dotfiles $argv
  end
end
