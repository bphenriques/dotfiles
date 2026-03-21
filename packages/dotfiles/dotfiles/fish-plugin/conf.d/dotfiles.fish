if test -z "$DOTFILES_CMD"
  set -U DOTFILES_CMD dot
end

if test -z "$DOTFILES_LOCATION"
  set -U DOTFILES_LOCATION "$HOME/.dotfiles"
end

if test ! -z $DOTFILES_CMD
  function $DOTFILES_CMD -d "dotfiles"
    __dotfiles $argv
  end
end