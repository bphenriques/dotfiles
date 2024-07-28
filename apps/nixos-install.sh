#!/usr/bin/env sh

# https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/scripts/install-system/install-system.sh

usage() {
  echo "Setups a nixos machine

Usage: nixos-installer.sh [build|install]

  --host            matching directory name under  match on of the directories under $DOTFILES_LOCATION/hosts/

Options:
"
}

append_if_absent() { touch "$2" && grep --quiet --fixed-strings -- "$1" "$2" || echo "$2" >> "$2"; }

confirm_y() {
  echo "WARNING! The disks in $TARGET_HOST are about to get wiped"
  echo "         NixOS will be re-installed"
  echo "         This is a destructive operation"
  echo
  read -p "Are you sure? [y/N]" -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
  fi
}

available_hosts() {
  if [[ -z "$TARGET_HOST" ]]; then
      echo "ERROR! $(basename "$0") requires a hostname as the first argument"
      echo "       The following hosts are available"
      find nixos -mindepth 2 -maxdepth 2 -type f -name default.nix | cut -d'/' -f2 | grep -v iso
      exit 1
  fi
}

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

while [ $# -gt 0 ]; do
  case "$1" in
    build)
      shift 1
      nix build ".#nixosConfigurations.$1.config.system.build.toplevel" --show-trace
      ;;
    install)
      shift 1
      flake_target="$1"
      ssh_host="$2"
      shift 2

      sops_age_private_key=
      sops_age_destination=
      while [ $# -gt 0 ]; do
        case "$1" in
          --help)                     usage;                          exit  0 ;;
          --sops-age-private_key)     sops_age_private_key="$2";      shift 2 ;;
          --sops-age-destination)     sops_age_destination="$2";      shift 2 ;;
          *) break ;;
        esac
      done
      test -z "${host}" && error "host is not set!"     && usage && exit 1
      test -z "${sops_age_private_key}" && test -z "${sops_age_destination}" && error "sops-age-* have to be both set!" && usage && exit 1

      file_tree_to_copy="$(mktemp)"
      if ! test -z "${sops_age_destination}"; then
        mkdir -p "$(dirname "${file_tree_to_copy}/${sops_age_destination}")"
        echo "$sops_age_private_key" >> "${file_tree_to_copy}/${sops_age_destination}"

        # TODO: should I run this only after the file is copied?
        #chmod 600 "${file_tree_to_copy}/${sops_age_destination}"
      fi

      tree file_tree_to_copy
      #nix run github:nix-community/nixos-anywhere -- --extra-files "$file_tree_to_copy" --flake "#.host" "${ssh_host}"
      ;;
    *)
      usage
      exit 1
      ;;
  esac
done

# https://github.com/wimpysworld/nix-config/blob/c44a1bd13868e759bb215f54ca1f3fe49eba6dae/scripts/install.sh


# https://github.com/wimpysworld/nix-config/blob/main/nixos/_mixins/scripts/install-system/install-system.sh
