#!/usr/bin/env bash
set -e

DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
HOST_FILE_LOCATION="${DOTFILES_LOCATION}/.nix-host"
[ ! -f "$HOST_FILE_LOCATION" ] && echo "$HOST_FILE_LOCATION not found" && exit 1
CURRENT_HOST="$(cat "$HOST_FILE_LOCATION")"
SHARED_HOSTS_CONFIGURATION="${DOTFILES_LOCATION}/hosts/shared.nix"
OS="$(uname -s)"

# Compute system type from OS
SYSTEM_TYPE=
case "$OS" in
  Darwin) SYSTEM_TYPE="darwin" ;;
  Linux)
    if [ -d /etc/nixos ]; then
      SYSTEM_TYPE="nixos"
    fi
    # home-manager only installs are not supported (yet)
    ;;
esac
[ -z "$SYSTEM_TYPE" ] && echo "Unsupported OS: $OS" && exit 1

usage() {
  echo "dotfiles [s]ync [--boot] [--test] [--dry-activate] | [u]pdate | [b]uild | [d]eploy <host> [--boot] | [o]ptimise | [r]epair | changelog | current-system"
}

have_cmd() { command -v "$1" >/dev/null 2>&1; }
info() { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }
fatal() { printf '[FAIL] %s\n' "$1" 1>&2; exit 1; }


nix_build() {
  if have_cmd nom; then
    nom build "$@"
  else
    nix --extra-experimental-features 'nix-command flakes' build "$@"
  fi
}

# Extracts host IP from hosts/shared.nix using nix eval
get_host_ip() { nix eval --raw --file "${SHARED_HOSTS_CONFIGURATION}" "networks.main.hosts.$1" || fatal "Could not find IP for host '$1'."; }

_nixos_build() { nix_build ".#nixosConfigurations.$CURRENT_HOST.config.system.build.toplevel"; }
_nixos_sync() {
  case "${1:-}" in
    --boot)           sudo nixos-rebuild boot --flake ".#$CURRENT_HOST"           ;;
    --test)           sudo nixos-rebuild test --flake ".#$CURRENT_HOST"           ;;
    --dry-activate)   sudo nixos-rebuild dry-activate --flake ".#$CURRENT_HOST"   ;;
    "")               sudo nixos-rebuild switch --flake ".#$CURRENT_HOST"         ;;
    *)                usage && exit 1                                             ;;
  esac
}

_darwin_build() { nix_build ".#darwinConfigurations.$CURRENT_HOST.system"; }
_darwin_sync() {
  _darwin_build "$@"
  ./result/sw/bin/darwin-rebuild switch --flake ".#$CURRENT_HOST"
}
_darwin_update() {
  if have_cmd brew; then
    brew update && brew upgrade
  else
    info "Homebrew not installed; skipping brew update/upgrade"
  fi
}

# Remote deployment using nixos-rebuild --target-host
# Note: For health checks and auto-rollback, consider deploy-rs: https://github.com/serokell/deploy-rs
_nixos_deploy() {
  local host="$1"
  local target_ip
  target_ip=$(get_host_ip "$host")

  info "Deploying '$host' to root@${target_ip}..."
  case "${2:-}" in
    --boot) nixos-rebuild boot   --flake ".#${host}" --target-host "root@${target_ip}" --use-remote-sudo ;;
    "")     nixos-rebuild switch --flake ".#${host}" --target-host "root@${target_ip}" --use-remote-sudo ;;
    *)      usage && exit 1 ;;
  esac
}

cd "$DOTFILES_LOCATION" || fatal "Failed to set the current directory"
case "${1:-}" in
  sync|s)
    shift 1
    info "Dotfiles Sync - '$CURRENT_HOST' ($SYSTEM_TYPE) .."
    case "$SYSTEM_TYPE" in
      darwin) _darwin_sync "$@" ;;
      nixos)  _nixos_sync "$@" ;;
    esac
    ;;
  update|u)
    info "Dotfiles Update - '$CURRENT_HOST' ($SYSTEM_TYPE) .."
    nix --extra-experimental-features 'nix-command flakes' flake update
    case "$SYSTEM_TYPE" in
      darwin) _darwin_update ;;
      nixos)  ;; # Nothing extra needed for NixOS
    esac
    ;;
  build|b)
    case "$SYSTEM_TYPE" in
      darwin) _darwin_build ;;
      nixos)  _nixos_build ;;
    esac
    ;;
  deploy|d)
    shift 1
    host="${1:-}"
    [ -z "$host" ] && fatal "Usage: dotfiles deploy <host> [--boot]"
    _nixos_deploy "$host" "${2:-}"
    ;;
  optimise|o)
    info "Optimizing Nix Store - GC + Dedup"
    nix store gc
    nix store optimise
    success "Optimizing Nix Store"
    ;;
  repair|r) sudo nix-store --repair --verify --check-contents ;;
  current-system) nix-store -qR /run/current-system | sed -n -e 's/\/nix\/store\/[0-9a-z]\{32\}-//p' | sort | uniq ;;
  changelog)
    if ! have_cmd nvd; then
      fatal "changelog requires 'nvd'. Install it or run via the flake."
    fi
    # shellcheck disable=SC2012,SC2086
    profiles="$(ls -dv /nix/var/nix/profiles/system-*-link 2>/dev/null | tail -2)"
    if [ "$(echo "$profiles" | wc -l)" -ge 2 ]; then
      # shellcheck disable=SC2086
      nvd diff $profiles
    else
      info "Not enough profiles to compare"
    fi
    ;;
  *) usage && exit 0 ;;
esac
