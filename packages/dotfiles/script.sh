#!/usr/bin/env sh
# shellcheck disable=SC1091
set -ef

DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
HOST_FILE_LOCATION="${DOTFILES_LOCATION}/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && echo "$HOST_FILE_LOCATION not found" && exit 1
CURRENT_HOST="$(cat "$HOST_FILE_LOCATION")"

usage() {
  echo "dotfiles [s]ync | [u]pdate | [b]uild | [o]ptimise | [r]epair | [f]ormat | [d]escribe | changelog"
}

info() { printf '[ \033[00;34m  \033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2 && exit 1; }

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
}

_nixos_sync() {
  pushd "${DOTFILES_LOCATION}" > /dev/null
  case "$1" in
    --boot)           sudo nixos-rebuild boot --flake ".#$CURRENT_HOST"           ;;
    --test)           sudo nixos-rebuild test --flake ".#$CURRENT_HOST"           ;;
    --dry-activate)   sudo nixos-rebuild dry-activate --flake ".#$CURRENT_HOST"   ;;
    "")               sudo nixos-rebuild switch --flake ".#$CURRENT_HOST"         ;;
    *)                error "Invalid switch $1" && usage && exit 1                ;;
  esac
  popd > /dev/null
}

_flake_update() {
  pushd "${DOTFILES_LOCATION}" > /dev/null
  nix --extra-experimental-features 'nix-command flakes' flake update
  popd > /dev/null
}

_init_fish() {
  echo 'function dotfiles
  if test (count $argv) -eq 0
    cd $DOTFILES_LOCATION
  else
    command dotfiles $argv
  end
end'
}

cd "$DOTFILES_LOCATION" || fatal "Failed to set the current directory"

case "${1:-}" in
  sync | s)
    shift 1

    info "Dotfiles Sync - '$CURRENT_HOST' .."
    case "$(uname -s)" in
      Darwin)  _darwin_sync "$@" ;;
      Linux)
        if [ -d /etc/nixos ]; then
          _nixos_sync "$@"
        else
          fatal "Not available outside NixOS"
        fi
        ;;
      *)  fatal "Unsupported Operating System: $(uname -s)" ;;
    esac
    ;;
  update | u)
    info "Dotfiles Update - '$CURRENT_HOST' .."
    # Relevant docs: https://nix.dev/manual/nix/2.18/installation/upgrading#upgrading-nix
    _flake_update
    case "$(uname -s)" in
      Darwin)  _darwin_update ;;
      Linux)
        if [ ! -d /etc/nixos ]; then
          nix-channel --update
          nix-env --install --attr nixpkgs.nix nixpkgs.cacert
          systemctl daemon-reload
          systemctl restart nix-daemon
        fi
        ;;
      *)  fatal "Unsupported Operating System: $(uname)" ;;
    esac
    ;;
  build  | b)
    case "$(uname -s)" in
      Darwin) _darwin_build ;;
      Linux)
        if [ -d /etc/nixos ]; then
          nix build ".#nixosConfigurations.$CURRENT_HOST.config.system.build.toplevel" --show-trace
        fi
        ;;
      *)  fatal "Unsupported Operating System: $(uname)" ;;
    esac
    ;;
  optimise | o)
    info "Optimizing Nix Store - GC + Dedup"
    nix store gc
    nix store optimise
    success "Optimizing Nix Store"
    ;;
  repair | r) sudo nix-store --repair --verify --check-contents ;;
  describe | d) nix-store -qR /run/current-system | sed -n -e 's/\/nix\/store\/[0-9a-z]\{32\}-//p' | sort | uniq ;;
  changelog)
    # nvd diff $(ls -dv /nix/var/nix/profiles/system-*-link | tail -2)
    # Improvements are blocked by https://github.com/NixOS/nix/issues/6129
    nix profile diff-closures --profile /nix/var/nix/profiles/system
    ;;
  --init-shell)
    shift 1
    case "${1:-}" in
      fish) _init_fish                            ;;
      *)    echo "Unsupported shell: $1"; exit 1  ;;
    esac
    ;;
  *) usage && exit 0 ;;
esac
