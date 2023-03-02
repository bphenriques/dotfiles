LOCATION="$HOME/.dotfiles"

__dotfiles_usage() {
   echo "dotfiles [o]pen"
   echo "dotfiles [e]dit"
   echo "dotfiles [s]ync"
   echo "dotfiles [u]pdate"
   echo "dotfiles [c]hangelog"
}

__dotfiles_open() {
  cd "$LOCATION"
}

__dotfiles_edit() {
  $EDITOR "$LOCATION"
}

__dotfiles_sync() (
  cd "$LOCATION"
  "$LOCATION"/bin/sync.sh
)

__dotfiles_update() (
  "$LOCATION"/bin/update.sh
)

__dotfiles_changelog() (
  nix profile diff-closures --profile /nix/var/nix/profiles/system
)

if [ $# -eq 0 ]; then
    __dotfiles_open
else
  while [ "$1" != "" ]; do
      case $1 in
          o | open)
                              shift
                              __dotfiles_open
                              ;;
          e | edit)
                              shift
                              __dotfiles_edit
                              ;;
          s | sync)
                              shift
                              __dotfiles_sync
                              ;;
          u | update)
                              shift
                              __dotfiles_update
                              ;;
          c | changelog)
                              shift
                              __dotfiles_changelog
                              ;;
          *)
                              __dotfiles_usage
                              return
        ;;
    esac
  done
fi
