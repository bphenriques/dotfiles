LOCATION="$HOME/.dotfiles"

__dotfiles_usage() {
   echo "dotfiles [o]pen"
   echo "dotfiles [e]dit"
   echo "dotfiles [s]ync"
   echo "dotfiles [u]pdate"
}

__dotfiles_open() {
  cd "$LOCATION"
}

__dotfiles_edit() {
  $EDITOR "$LOCATION"
}

__dotfiles_sync() (
  cd "$LOCATION"
  make sync
)

__dotfiles_update() (
  cd "$LOCATION"
  make update
)

if [ $# -eq 0 ]; then
    __dotfiles_open
else
  while [ "$1" != "" ]; do
      case $1 in
          o | open )          shift
                              __dotfiles_open
                              ;;
          e | edit )          shift
                              __dotfiles_edit
                              ;;
          s | sync )          shift
                              __dotfiles_sync
                              ;;
          u | update )        shift
                              __dotfiles_update
                              ;;
          * )                 __dotfiles_usage
                              exit 1
      esac
  done
fi