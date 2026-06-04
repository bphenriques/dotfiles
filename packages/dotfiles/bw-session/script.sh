#shellcheck shell=bash

set -e

# shellcheck disable=SC2016
usage() {
  cat <<'EOF'
Log in to and unlock Bitwarden CLI.

sh:   export BW_SESSION="$(bw-session session <EMAIL>)"
fish: set -x BW_SESSION (bw-session session <EMAIL>)

Escape hatch: if BW_SESSION is already set in the environment (e.g.
you ran `bw unlock --raw` manually), this command echoes it back
unchanged and skips the login/unlock/sync dance — useful when bw's
interactive password prompt misbehaves or its implicit sync hangs.
EOF
}

# Adapted from https://gist.github.com/seanh/d3d1a6dfa4b7d5d9f135984ae913cf0f
create_session() {
  test -z "$1" && echo "EMAIL not provided" && exit 1

  # Fast path: caller already has a session token. Trust it, skip
  # login/unlock checks (which themselves can hang on bw's implicit
  # sync) and the explicit `bw sync` below.
  if [ -n "${BW_SESSION:-}" ]; then
    echo "$BW_SESSION"
    return
  fi

  if ! bw login --check >/dev/null; then
    BW_SESSION="$(bw login --raw "$1")"
  fi

  if ! bw unlock --check >/dev/null; then
    BW_SESSION="$(bw unlock --raw)"
  fi

  bw sync >/dev/null
  echo "$BW_SESSION"
}

case "${1:-}" in
  "" | --help) usage ;;
  session) shift 1 && create_session "$@" ;;
esac
