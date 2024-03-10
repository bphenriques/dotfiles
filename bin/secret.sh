#!/usr/bin/env sh
set -euf

_secret_usage() {
  echo '
  secret.sh [e]dit <file>
  secret.sh public-key
'
}

_secret_get_public_key() {
  nix-shell -p ssh-to-age --run "ssh-to-age < \"$HOME/.ssh/id_ed25519.pub\""
}

_secret_edit() {
  nix-shell -p sops --run "sops $1"
}

case $1 in
  e|edit)       shift && _secret_edit "$1"  ;;
  public-key)   _secret_get_public_key      ;;
  *)            _secret_usage               ;;
esac

