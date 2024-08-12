#!/usr/bin/env sh

# https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/scripts/install-system/install-system.sh
# Like this one a lot: https://github.com/panchoh/nixos

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }

info "\nWelcome to bphenriques\'s nixos-installer"

echo -e '\nGreetings, human with ID: "${userDesc} (${userName}) <${userEmail}>", GitHub account: "${githubUser}".'
echo -e "\nYou have requested the priming of the box designated as '${hostName}', on device: '${diskDevice}'."


format() {
  sudo true

  sudo nix run github:nix-community/disko \
    --extra-experimental-features "nix-command flakes" \
    --no-write-lock-file \
    -- \
    --mode zap_create_mount \
    "nixos/$TARGET_HOST/disks.nix"

  sudo nixos-install --no-root-password --flake ".#$TARGET_HOST"
}

confirm_y() {
  host="$1"
  echo "WARNING! The disks in $host are about to get wiped"
  echo "         NixOS will be re-installed"
  echo "         This is a destructive operation"
  echo
  read -p "Are you sure? [y/N]" -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
  fi
}
