#! /usr/bin/env nix-shell
#! nix-shell -i bash -p sops
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/6a8a86ede2e8ce496648f2a8ae79d2c24464ca2a.tar.gz
# shellcheck shell=sh disable=SC2046

BASE_NAME=sopsGitFilter

if [ "$EUID" -eq 0 ]; then
  echo "Skipping sops git filter as it is running as root likely as part of nixos build. This is only meant to run by users."
  exit 1
fi

case $1 in
  init)
    shift 1
    host="$1"
    if test -z "$host" || ! test -d "hosts/$host"; then
      echo "No matching host for: '$host'"
      exit 2
    fi
    git config --local filter.${BASE_NAME}${host}.required true
    git config --local filter.${BASE_NAME}${host}.smudge './bin/sops-git-filter.sh smudge "%f"'
    git config --local filter.${BASE_NAME}${host}.clean './bin/sops-git-filter.sh clean "%f"'
    ;;
  smudge) shift 1 && sops decrypt --input-type json --output-type binary --filename-override "$1" /dev/stdin  ;;
  clean)  shift 1 && sops encrypt --input-type binary --output-type json --filename-override "$1" /dev/stdin  ;;
esac
