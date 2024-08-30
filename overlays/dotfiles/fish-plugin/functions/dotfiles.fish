function dotfiles -d 'Interactive dotfiles'
  if test (count $argv) -eq 0
    cd $DOTFILES_LOCATION
  else
    $DOTFILES_LOCATION/overlays/dotfiles/dotfiles.sh $argv
  end
end
