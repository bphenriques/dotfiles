#!/usr/bin/env sh
# shellcheck shell=sh disable=SC2046,SC3028
DOTFILES_LOCATION="${DOTFILES_LOCATION:-"$HOME"/.dotfiles}"
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
  git config --local "filter.${BASE_NAME}${host}.required" false
  git config --local "filter.${BASE_NAME}${host}.smudge" './bin/sops-git-filter.sh smudge "%f"'
  git config --local "filter.${BASE_NAME}${host}.clean" './bin/sops-git-filter.sh clean "%f"'
  git rm -rf "hosts/${host}"/*
  git checkout HEAD "hosts/${host}"
}

smudge() {
  sops decrypt --input-type json --output-type binary --filename-override "$1" /dev/stdin
}

clean() {
  sops encrypt --input-type binary --output-type json --filename-override "$1" /dev/stdin
}


if [ "$EUID" -eq 0 ]; then
  echo "Skipping sops git filter as it is running as root likely as part of nixos build. This is only meant to run by users."
  exit 0
fi

case $1 in
  init)   shift 1 && init "$1"    ;;
  smudge) shift 1 && smudge "$1"  ;;
  clean)  shift 1 && clean "$1"   ;;
esac
