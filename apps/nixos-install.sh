#!/usr/bin/env sh

usage() {
  echo "Setups a nixos machine

Usage: nixos-installer.sh [test|nixos-anywhere|disko]

  --host            matching directory name under  match on of the directories under $DOTFILES_LOCATION/hosts/

Options:
"
}

append_if_absent() { touch "$2" && grep --quiet --fixed-strings -- "$1" "$2" || echo "$2" >> "$2"; }

nixos_build() {
  nix build ".#nixosConfigurations.$1.config.system.build.toplevel" --show-trace
}

nixos_remote_install() {
  flake_target="$1"
  host_target="$2"
  file_tree_to_copy="$3"

  nix run github:nix-community/nixos-anywhere -- --extra-files "$file_tree_to_copy" --flake "${flake_target}" "${host_target}"
}

while [ $# -gt 0 ]; do
  case "$1" in
    build)
      shift 1
      nixos_build "$1"
      ;;
    remote-install)
      shift 1
      host=
      sops_age_private_key=
      sops_age_destination=
      while [ $# -gt 0 ]; do
        case "$1" in
          --help)                     usage;                          exit  0 ;;
          --flake-target)             flake_target="$2";              shift 2 ;;
          --ssh-host)                 ssh_host="$2";                  shift 2 ;;
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
      fi

      nixos_remote_install "$flake_target" "${ssh_host}" "${file_tree_to_copy}"
      ;;
    *)
      usage
      exit 1
      ;;
  esac
done


