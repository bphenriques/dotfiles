#shellcheck shell=bash

# nvchecker resolves oldver/newver next to its config, so run from a writable copy.
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp "$NVCHECKER_CONFIG" "$work/nvchecker.toml"
cp "$NVCHECKER_OLDVER" "$work/oldver.json"

keyfile=()
if [[ -n ${GITHUB_TOKEN:-} ]]; then
  printf '[keys]\ngithub = "%s"\n' "$GITHUB_TOKEN" >"$work/keys.toml"
  keyfile=(--keyfile "$work/keys.toml")
fi

status=0
if ! nvchecker --file "$work/nvchecker.toml" --failures --logging error "${keyfile[@]}"; then
  echo "Could not check every pin, so this run is incomplete." >&2
  status=1
fi
if ! nvcmp --file "$work/nvchecker.toml" --all --exit-status; then
  echo "Some pinned versions are outdated. Update the versions in overlays/."
  status=1
fi
exit "$status"
