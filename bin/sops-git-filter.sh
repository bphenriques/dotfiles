#! /usr/bin/env sh
# **Not part of my flake intentionally** as:
# - It is not really an application, but more like an helper when interacting with the remote repository.
# - Unstable due flake's interactions with this repository which would require calling this "app" (as sudo for nixos rebuilds!)
#
# **Not using https://nixos.wiki/wiki/Nix-shell_shebang for the same reason but also**
# - Too slow :( 766ms while using nix-shell pinned to a nixpkgs version. 12ms if using the binary directly.
#
# Solution: using nix develop environments.
BASE_NAME=sopsGitFilter

init() {
  host="$1"

  if git config --get "filter.${BASE_NAME}${host}.required" && git config --get "filter.${BASE_NAME}${host}.smudge" && git config --get "filter.${BASE_NAME}${host}.clean" > /dev/null; then
    echo "Already initialized. Skipping."
    return 0
  fi

  if ! cd "$DOTFILES_LOCATION"; then
    echo "Failed enter dotfiles directory as it does not exist!"
    exit 2
  fi
  git config --local "filter.${BASE_NAME}${host}.required" true
  git config --local "filter.${BASE_NAME}${host}.smudge" './sops-git-filter.sh smudge "%f"'
  git config --local "filter.${BASE_NAME}${host}.clean"  './sops-git-filter.sh clean  "%f"'
  git rm -rf "hosts/${host}"/*
  git checkout HEAD "hosts/${host}"
}

smudge() { sops decrypt --input-type json --output-type binary --filename-override "$1" /dev/stdin ; }
clean() { sops encrypt --input-type binary --output-type json --filename-override "$1" /dev/stdin ; }

if ! command -v sops > /dev/null 2>&1; then
  echo "'sops' command is not present in the current \$PATH."
  exit 1
fi

case $1 in
  init)   shift 1 && init "$1"    ;;
  smudge) shift 1 && smudge "$1"  ;;
  clean)  shift 1 && clean "$1"   ;;
esac
