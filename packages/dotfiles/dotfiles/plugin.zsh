dot() {
  if (( $# == 0 )); then
    cd "$DOTFILES_LOCATION"
  else
    command dotfiles "$@"
  fi
}
