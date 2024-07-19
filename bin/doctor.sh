#!/usr/bin/env sh
set -uf

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.
DOTFILES_LOCATION="$HOME/.dotfiles"
HOST_FILE_LOCATION="$DOTFILES_LOCATION"/.nix-host

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

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

verify_sops_secrets() {
  info 'Sops secrets - Checking...'
  if [ ! -f "$XDG_CONFIG_HOME/sops/age/keys.txt" ]; then
    fail "Missing Sops secrets file: $XDG_CONFIG_HOME/sops/age/keys.txt"
  else
    success "Sops secrets present in $XDG_CONFIG_HOME/sops/age/keys.txt"
  fi
}

assert_installed() {
  if command -v "$1" >/dev/null; then
    success "$1 - Installed!"
  else
    fail "$1 - Not installed!"
  fi
}

case "$(uname -s)" in
  Darwin)
    assert_installed brew
    ;;
  *)
    ;;
esac

check_shell

if test -d "$DOTFILES_LOCATION/host/$(cat "$HOST_FILE_LOCATION")"; then
  success "Nix Host - Set to '$(cat "$HOST_FILE_LOCATION")'!"
else
  fail "Nix Host - Invalid host! It is $(cat "$HOST_FILE_LOCATION")"
fi

verify_sops_secrets

if test -f "$HOME/.ssh/id_ed25519"; then
  success "SSH Key: id_ed25519 set"
else
  fail "SSH Key: missing id_ed25519 key: $HOME/.ssh/id_ed25519"
fi

"$SCRIPT_PATH"/git-secret-filter.sh doctor
