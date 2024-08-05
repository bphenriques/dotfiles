function dotfiles -d 'Interactive dotfiles'
  if test (count $argv) -eq 0
    if set -q DOTFILES_LOCATION
      cd $DOTFILES_LOCATION
    else
      echo "DOTFILES_LOCATION is not set. Nothing to do."
    end
  else
    dotfiles $argv
  end
end
