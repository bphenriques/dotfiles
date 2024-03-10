#! /usr/bin/env nix-shell
#! nix-shell -i bash -p age
# shellcheck shell=sh

case $1 in
  init)
    git config --local filter.ageencrypt.smudge "./bin/git-secret-filter.sh smudge"
    git config --local filter.ageencrypt.clean "./bin/git-secret-filter.sh clean"
    git config --local filter.ageencrypt.required true
    ;;
  smudge)
    shift
    age --decrypt --identity "$HOME/.ssh/id_ed25519" -
    ;;
  clean)
    shift
    age --recipients-file .age-recipients --armor -
    ;;
esac

