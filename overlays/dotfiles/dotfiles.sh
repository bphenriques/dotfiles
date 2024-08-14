#!/usr/bin/env sh
# shellcheck disable=SC1091
set -ef

DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
HOST_FILE_LOCATION="${DOTFILES_LOCATION}/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && echo "$HOST_FILE_LOCATION not found" && exit 1
CURRENT_HOST="$(cat "$HOST_FILE_LOCATION")"

usage() {
  echo "something"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2 && exit 1; }
press_to_continue() { info 'Press any key to continue' && read -r _; }

_darwin_sync() {
  pushd "${DOTFILES_LOCATION}" > /dev/null
  _darwin_build "$@"
  ./result/sw/bin/darwin-rebuild switch --flake ".#$CURRENT_HOST"
  popd > /dev/null
}

_darwin_build() {
  pushd "${DOTFILES_LOCATION}" > /dev/null
  nix build --extra-experimental-features 'nix-command flakes' ".#darwinConfigurations.$CURRENT_HOST.system" --show-trace
  popd > /dev/null
}

_darwin_update() {
  brew upgrade && brew update
  # FIXME: No longer works
  # sudo -i sh -c 'nix-channel --update && nix-env --install --attr nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
}

_nixos_sync() {
  pushd "${DOTFILES_LOCATION}" > /dev/null
  case "$1" in
    --boot)   sudo nixos-rebuild boot --flake ".#$CURRENT_HOST"     ;;
    --test)   sudo nixos-rebuild test --flake ".#$CURRENT_HOST"     ;;
    --check)  nixos-rebuild dry-activate --flake ".#$CURRENT_HOST"  ;;
    "")        sudo nixos-rebuild switch --flake ".#$CURRENT_HOST"  ;;
  esac
  popd > /dev/null
}

_flake_update() {
  pushd "${DOTFILES_LOCATION}" > /dev/null
  nix --extra-experimental-features 'nix-command flakes' flake update
  popd > /dev/null
}

while [ $# -gt 0 ]; do
  case "$1" in
    sync | s)
      shift 1

      info "Dotfiles Sync - '$CURRENT_HOST' .."
      case "$(uname -s)" in
        Darwin)  _darwin_sync "$@" ;;
        Linux)
          if [ -d /etc/nixos ]; then
            _nixos_sync "$@"
          else
            fail "Not available outside NixOS"
          fi
          ;;
        *)  fail "Unsupported Operating System: $(uname -s)" ;;
      esac
      ;;
    update | u)
      info "Dotfiles Update - '$CURRENT_HOST' .."
      # Relevant docs: https://nix.dev/manual/nix/2.18/installation/upgrading#upgrading-nix
      _flake_update
      case "$(uname -s)" in
        Darwin)  _darwin_update "$@" ;;
        Linux)
          if [ -d /etc/nixos ]; then
            _nixos_update "$@"
          else
            nix-channel --update
            nix-env --install --attr nixpkgs.nix nixpkgs.cacert
            systemctl daemon-reload
            systemctl restart nix-daemon
          fi
          ;;
        *)  fail "Unsupported Operating System: $(uname)" ;;
      esac
      ;;
    optimise | o)
      shift 1
      echo "Running garbage collection"
      nix store gc
      echo "Deduplication running... may take a while"
      nix store optimise
      ;;
    repair | r)
      shift 1
      sudo nix-store --repair --verify --check-contents
      ;;
    format | f)
      shift 1
      nix-shell -p fd nixpkgs-fmt --command "fd -e nix -E '/nix/sources.nix' -E 'hardware-configuration*' -x nixpkgs-fmt \"{}\" \;"
      ;;
    describe-current-system)
      nix-store -qR /run/current-system | sed -n -e 's/\/nix\/store\/[0-9a-z]\{32\}-//p' | sort | uniq
      ;;
    changelog)
      # nvd diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2)
      # Improvements are blocked by https://github.com/NixOS/nix/issues/6129
      # nix profile diff-closures --profile /nix/var/nix/profiles/system
      ;;
    *) usage && exit 0 ;;
  esac
done
