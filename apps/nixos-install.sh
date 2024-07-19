#!/usr/bin/env sh

usage() {
  echo "Setups a nixos machine

Usage: nixos-installer.sh [test|nixos-anywhere|disko]

  --host            matching directory name under  match on of the directories under $DOTFILES_LOCATION/hosts/

Options:
"
}

nixos_build() {
  nix build ".#nixosConfigurations.$1.config.system.build.toplevel" --show-trace
}

nixos_remote_install() {
  nix run github:nix-community/nixos-anywhere -- --extra-files "$temp" --flake "${TARGET}" "${HOST}"
}

while [ $# -gt 0 ]; do
  case "$1" in
    build)               mode=build;                    shift 1 ;;
    remote-install)     mode=remote-install;          shift 1 ;;
    format-disk)        mode=format-disk;             shift 1 ;;
    *)                  usage;                        exit 1  ;;
  esac
done

host=
while [ $# -gt 0 ]; do
  case "$1" in
    --help)             usage;                  exit  0 ;;
    --host)             host="$2";              shift 2 ;;
    *) break ;;
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

temp="$(mktemp -d)"
if ! "$SCRIPT_PATH"/init-keys.sh "$temp/$HOME_DIRECTORY"; then
  fatal "Failed to initialize keys."
fi

# TODO: Arguments: --secret {id}
# /persist/config/bphenriques/home/bphenriques




