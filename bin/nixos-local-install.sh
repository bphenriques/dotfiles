#!/usr/bin/env sh



# https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/scripts/install-system/install-system.sh
# https://github.com/wimpysworld/nix-config/blob/c44a1bd13868e759bb215f54ca1f3fe49eba6dae/scripts/install.sh
# https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/scripts/install-system/install-system.sh
# https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/scripts/install-system/install-system.sh

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
