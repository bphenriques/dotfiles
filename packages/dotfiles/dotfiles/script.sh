#shellcheck shell=bash

usage() {
  cat <<EOF
dot <host|.> <command>

  '.' is this machine. Any other host is deployed as root@<host>.

  [s]ync [--boot|--test]   Build, show the diff and apply
  [b]uild                  Build and show the diff [local]
  [c]hangelog [N]          Diff the last N generations (default: 2)
  [u]pdate                 Update flake inputs, and Homebrew on macOS [local]
EOF
}

fatal() {
  printf '[FAIL] %s\n' "$1" >&2
  exit 1
}

action() {
  case "${1:-}" in
    "") echo switch ;;
    --boot) echo boot ;;
    --test) echo test ;;
    *) fatal "Unknown sync flag: $1" ;;
  esac
}

# Args after the count run each command, locally (bash -c) or remotely (ssh root@host).
changelog() {
  local count="${1:-2}" gens i
  shift
  [[ $count =~ ^[1-9][0-9]*$ ]] || fatal "Changelog count must be a positive integer"
  mapfile -t gens < <("$@" 'ls -dv /nix/var/nix/profiles/system-*-link' | tail -n "$count")
  for ((i = 1; i < ${#gens[@]}; i++)); do
    "$@" "nvd diff ${gens[i - 1]} ${gens[i]}"
  done
}

[[ $# -ge 2 ]] || {
  usage
  exit 0
}
target="$1"
command="$2"
shift 2

: "${NH_FLAKE:?NH_FLAKE is unset; it comes from programs.nh.flake}"
platform=os
[[ $(uname -s) == Darwin ]] && platform=darwin

if [[ $target == . ]]; then
  case "$command" in
    s | sync) act=$(action "$@") && nh "$platform" "$act" ;;
    b | build) nh "$platform" build ;;
    c | changelog) changelog "${1:-}" bash -c ;;
    u | update)
      nix flake update --flake "$NH_FLAKE"
      if [[ $platform == darwin ]]; then brew update && brew upgrade; fi
      ;;
    *) fatal "Unknown command: $command" ;;
  esac
else
  case "$command" in
    s | sync) act=$(action "$@") && nh os "$act" --hostname "$target" --target-host "root@$target" ;;
    c | changelog) changelog "${1:-}" ssh "root@$target" ;;
    *) fatal "'$command' only runs on this machine: dot . $command" ;;
  esac
fi
