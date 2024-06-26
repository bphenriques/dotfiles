#!/usr/bin/env sh
# shellcheck disable=SC1091,SC2181
set -uf
SCRIPT_PATH="$(dirname "$0")"
# shellcheck source=util.sh
. "$SCRIPT_PATH"/util.sh

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.
DOTFILES_LOCATION="$HOME/.dotfiles"
HOST_FILE_LOCATION="$DOTFILES_LOCATION"/.nix-host

check_shell() {
  case $SHELL in
    */fish) success 'Shell - Already set correctly!' ;;
    *)
      location="$(which fish 2>/dev/null)"
      if [ $? -eq 0 ]; then
        warn "Fish is not the current shell! You may set using: chsh -s '$location'"
      else
        warn "Fish is not available in PATH"
      fi
      ;;
  esac
}

assert_installed() {
  if command -v "$1" >/dev/null; then
    success "$1 - Installed!"
  else
    fail "$1 - Not installed!"
  fi
}

assert_installed nix

if command -v nix flake show templates >/dev/null; then
  success 'Nix flake - Installed!'
else
  fail 'Nix flake - Not installed!'
fi

case "$(uname -s)" in
  Darwin)
    assert_installed brew
    ;;
  *) ;;
esac

check_shell

if test -d "$DOTFILES_LOCATION/host/$(cat "$HOST_FILE_LOCATION")"; then
  success "Nix Host - Set to '$(cat "$HOST_FILE_LOCATION")'!"
else
  fail "Nix Host - Invalid host! It is $(cat "$HOST_FILE_LOCATION")"
fi

if test -f "$XDG_CONFIG_HOME/sops/age/keys.txt"; then
  success "Sops Secrets - Keys present!"
else
  fail "Sops Secrets - Keys file missing: $XDG_CONFIG_HOME/sops/age/keys.txt"
fi

if test -f "$HOME/.ssh/id_ed25519"; then
  success "SSH Key: id_ed25519 set"
else
  fail "SSH Key: missing id_ed25519 key: $HOME/.ssh/id_ed25519"
fi

"$SCRIPT_PATH"/git-secret-filter.sh doctor
