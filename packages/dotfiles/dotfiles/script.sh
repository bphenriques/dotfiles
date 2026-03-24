#!/usr/bin/env bash
set -e

have_cmd() { command -v "$1" >/dev/null 2>&1; }
info() { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }
fatal() { printf '[FAIL] %s\n' "$1" >&2; exit 1; }

DOTFILES_LOCATION=${DOTFILES_LOCATION:-"$HOME/.dotfiles"}
HOST_FILE_LOCATION="$DOTFILES_LOCATION/.nix-host"
SHARED_HOSTS_CONFIGURATION="$DOTFILES_LOCATION/hosts/shared.nix"

[[ -f $HOST_FILE_LOCATION ]] || fatal "$HOST_FILE_LOCATION not found"
CURRENT_HOST=$(<"$HOST_FILE_LOCATION")

OS="$(uname -s)"
SYSTEM_TYPE=
case "$OS" in
  Darwin) SYSTEM_TYPE="darwin" ;;
  Linux)  [[ -d /etc/nixos ]] && SYSTEM_TYPE="nixos" ;; # home-manager only installs are not supported (yet)
esac
[[ -n $SYSTEM_TYPE ]] || fatal "Unsupported OS: $OS"

usage() {
  cat <<EOF
dotfiles <host|.> <command>

  '.' targets the current host ($CURRENT_HOST). Commands that only apply locally
  are marked with [local].

  [s]ync [--boot] [--test] [--dry-activate]   Apply configuration
  [c]hangelog [N]                              Show changelog between last N profiles (default: 2)
  [u]pdate                  [local]            Update flake inputs (and brew on Darwin)
  [b]uild                   [local]            Build configuration
  [o]ptimise                [local]            GC and deduplicate the Nix store
  [r]epair                  [local]            Repair the Nix store
  [i]nfo                    [local]            List packages in current system
EOF
}

NIX=(nix --extra-experimental-features 'nix-command flakes')

nix_build() {
  if have_cmd nom; then
    nom build "$@"
  else
    "${NIX[@]}" build "$@"
  fi
}

get_host_ip() { "${NIX[@]}" eval --raw --file "$SHARED_HOSTS_CONFIGURATION" "networks.main.hosts.$1" || fatal "Could not find IP for host '$1'."; }

_nixos_build() { nix_build ".#nixosConfigurations.$CURRENT_HOST.config.system.build.toplevel"; }
_nixos_sync() {
  case "${1:-}" in
    --boot)           sudo nixos-rebuild boot --flake ".#$CURRENT_HOST"           ;;
    --test)           sudo nixos-rebuild test --flake ".#$CURRENT_HOST"           ;;
    --dry-activate)   sudo nixos-rebuild dry-activate --flake ".#$CURRENT_HOST"   ;;
    "")               sudo nixos-rebuild switch --flake ".#$CURRENT_HOST"         ;;
    *)                usage; exit 1                                               ;;
  esac
}

_darwin_build() { nix_build ".#darwinConfigurations.$CURRENT_HOST.system"; }
_darwin_sync() {
  _darwin_build
  ./result/sw/bin/darwin-rebuild switch --flake ".#$CURRENT_HOST"
}
_darwin_update() {
  if have_cmd brew; then
    brew update && brew upgrade
  else
    info "Homebrew not installed; skipping brew update/upgrade"
  fi
}

# Note: For health checks and auto-rollback, consider deploy-rs: https://github.com/serokell/deploy-rs
_nixos_deploy() {
  local host="$1"
  local target_ip
  target_ip=$(get_host_ip "$host")

  info "Deploying '$host' to root@${target_ip}..."
  case "${2:-}" in
    --boot) nixos-rebuild boot   --flake ".#${host}" --target-host "root@${target_ip}" --use-remote-sudo ;;
    "")     nixos-rebuild switch --flake ".#${host}" --target-host "root@${target_ip}" --use-remote-sudo ;;
    *)      usage; exit 1 ;;
  esac
}

# Lists system profile paths, version-sorted. Args are an optional command prefix (e.g., ssh root@ip).
_list_profiles() {
  # shellcheck disable=SC2012
  if (( $# == 0 )); then
    ls -dv /nix/var/nix/profiles/system-*-link 2>/dev/null
  else
    "$@" 'ls -dv /nix/var/nix/profiles/system-*-link 2>/dev/null'
  fi
}

# Diffs the last N system generations. Args after count are an optional command prefix for remote execution.
_nvd_diff_generations() {
  local count="$1"
  shift

  [[ "$count" =~ ^[1-9][0-9]*$ ]] || fatal "Changelog count must be a positive integer"

  local -a profiles=()
  local line
  while IFS= read -r line; do
    [[ -n $line ]] && profiles+=("$line")
  done < <(_list_profiles "$@")

  local total=${#profiles[@]}
  if (( total < 2 )); then
    info "Not enough profiles to compare"
    return 0
  fi

  local start=0
  (( total > count )) && start=$((total - count))

  local i
  for ((i = start; i < total - 1; i++)); do
    "$@" nvd diff "${profiles[$i]}" "${profiles[$((i + 1))]}"
  done
}

cd "$DOTFILES_LOCATION" || fatal "Failed to set the current directory"

target="${1:-}"
[[ -n $target ]] || { usage; exit 0; }
shift

# Resolve '.' to the current host
[[ $target == "." ]] && target="$CURRENT_HOST"

is_local() { [[ $target == "$CURRENT_HOST" ]]; }
assert_local() { is_local || fatal "'$1' can only be run on the current host (use '.' or '$CURRENT_HOST')"; }

case "${1:-}" in
  sync|s)
    shift
    if is_local; then
      info "Dotfiles Sync - '$CURRENT_HOST' ($SYSTEM_TYPE) .."
      "_${SYSTEM_TYPE}_sync" "$@"
    else
      _nixos_deploy "$target" "${1:-}"
    fi
    ;;
  changelog|c)
    shift
    if is_local; then
      have_cmd nvd || fatal "changelog requires 'nvd'. Install it or run via the flake."
      _nvd_diff_generations "${1:-2}"
    else
      _nvd_diff_generations "${1:-2}" ssh "root@$(get_host_ip "$target")"
    fi
    ;;
  update|u)
    assert_local update
    info "Dotfiles Update - '$CURRENT_HOST' ($SYSTEM_TYPE) .."
    "${NIX[@]}" flake update
    case "$SYSTEM_TYPE" in
      darwin) _darwin_update ;;
    esac
    ;;
  build|b)
    assert_local build
    "_${SYSTEM_TYPE}_build"
    ;;
  optimise|o)
    assert_local optimise
    info "Optimizing Nix Store - GC + Dedup"
    nix store gc
    nix store optimise
    success "Optimizing Nix Store"
    ;;
  repair|r)
    assert_local repair
    sudo nix-store --repair --verify --check-contents
    ;;
  info|i)
    assert_local info
    nix-store -qR /run/current-system | sed -n 's|/nix/store/[0-9a-z]\{32\}-||p' | sort -u
    ;;
  *) usage; exit 1 ;;
esac
