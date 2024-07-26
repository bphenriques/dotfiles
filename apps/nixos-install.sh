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
  local home_directory="$1"
  local file_tree_to_copy="$(mktemp -d)"

  if ! "$SCRIPT_PATH"/init-keys.sh "$file_tree_to_copy/$home_directory"; then
    fatal "Failed to initialize keys."
  fi


  nix run github:nix-community/nixos-anywhere -- --extra-files "$temp" --flake "${TARGET}" "${HOST}"
}

initialize_age_key() {
  local target="$1"
  if [ -f "${target}" ]; then
    success 'Shared Age Key - Already present!'
  else
    info 'Shared Age Key - Adding shared key by getting it from Bitwarden!'
    check_bw_login
    mkdir -p "$(dirname "$target")"
    bw get item "$BITWARDEN_SHARED_KEY_ID" | jq --raw-output '.fields[] | select(.name=="private-key") | .value' >> "${target}"
    success 'SSH Key - Installed!'
  fi
}


check_bw_login() {
  if ! bw login --check >/dev/null; then
    echo "You are not logged in: bw login"
    return 1
  fi
  if ! bw unlock --check >/dev/null; then
    echo "The vault is locked: bw unlock"
    return 1
  fi
}

build() {

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
      sops_age_bitwarden_from=
      sops_age_destination=
      while [ $# -gt 0 ]; do
        case "$1" in
          --help)                     usage;                          exit  0 ;;
          --host)                     host="$2";                      shift 2 ;;
          --sops-age-bitwarden-from)  sops_age_bitwarden_from="$1";   shift 2 ;;
          --sops-age-destination)     sops_age_destination="$1";      shift 2 ;;
          *) break ;;
        esac
      done

      test -z "${host}"     && error "host is not set!"     && usage && exit 1
      if ! test -z "${sops_age_bitwarden_from}"; then
        test -z "${sops_age_destination}" && error "sops-age-destination is not set!" && usage && exit 1

        

      fi



    *)                  usage;                        exit 1  ;;
  esac
done



test -z "${host}"     && error "host is not set!"     && usage && exit 1

case "$mode" in)
  build) nixos_build "$host"  ;;
  remote-install) ;;
  format-disk)    ;;
  *)  ;;
esac
test -z "${ssh_host}" && error "ssh_host is not set!" && usage && exit 1

exit 0

# TODO: Arguments: --secret {id}
# /persist/config/bphenriques/home/bphenriques




sops-age-key-laptop
