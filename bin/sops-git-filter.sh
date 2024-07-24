#! /usr/bin/env nix-shell
#! nix-shell -i bash -p sops
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/6a8a86ede2e8ce496648f2a8ae79d2c24464ca2a.tar.gz
# shellcheck shell=sh disable=SC2046

FILTER_NAME=sopsGitFilter

validate_host() {
  local host="$1"
  if test -z "${host}"; then
    echo "host must not be empty!"
    exit 2
  elif ! test -d "hosts/${host}"; then
    echo "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"
    exit 3
  fi
}

init() {
  local host="$1"
  validate_host "$1"
  git config --local filter.${FILTER_NAME}${host}.required true
  git config --local filter.${FILTER_NAME}${host}.smudge './bin/sops-git-filter.sh smudge "%f"'
  git config --local filter.${FILTER_NAME}${host}.clean './bin/sops-git-filter.sh clean "%f"'
}

smudge() {
  sops decrypt --input-type json --output-type binary --filename-override "$1" /dev/stdin
}

clean() {
  sops encrypt --input-type binary --output-type json --filename-override "$1" /dev/stdin
}


if [ "$EUID" -eq 0 ]; then
  echo "Skipping sopsgitfilter as it is running as root likely as part of nixos build. This is only meant to run by users."
  exit 1
fi

case $1 in
  init)   shift 1 && init "$1"    ;;
  smudge) shift 1 && smudge "$1"  ;;
  clean)  shift 1 && clean "$1"   ;;
esac
