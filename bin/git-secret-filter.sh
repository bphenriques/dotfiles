#! /usr/bin/env nix-shell
#! nix-shell -i bash -p age -p yq-go
# shellcheck shell=sh disable=SC2046

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME"/.config} # Set if absent.
PRIVATE_KEYS_IDENTITY="$XDG_CONFIG_HOME/sops/age/keys.txt"
SOPS_FILE=".sops.yaml"

case $1 in
  init)
    git config --local filter.ageencrypt.required true
    git config --local filter.ageencrypt.smudge "./bin/git-secret-filter.sh smudge"
    git config --local filter.ageencrypt.clean "./bin/git-secret-filter.sh clean"
    ;;
  doctor)
    if ! (git config --get filter.ageencrypt.smudge && git config --get filter.ageencrypt.clean && git config --get filter.ageencrypt.required) > /dev/null; then
      echo "git-secret-filter not initialized. Run: ./bin/git-secret-filter.sh init"
      exit 1
    fi
    ;;
  smudge) shift && age --decrypt --identity "$PRIVATE_KEYS_IDENTITY" -                                            ;;
  clean)  shift && age $(yq '.keys[] | explode(.)' < "${SOPS_FILE}" | xargs -I{} echo '-r {}' | xargs) --output - ;;
esac
