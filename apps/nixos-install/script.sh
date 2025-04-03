#shellcheck shell=bash

BRANCH_NAME="${BRANCH_NAME:-main}"
FLAKE_URL="${FLAKE_URL:-github:bphenriques/dotfiles/${BRANCH_NAME}}"
DOTFILES_LOCATION="${DOTFILES_LOCATION:-$HOME/.dotfiles}"
SOPS_AGE_SYSTEM_FILE="/var/lib/sops-nix/system-keys.txt"

fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { echo 'Press any key to continue'; read -r _; }

dotfiles_sops_contains_host() { yq '.keys[] | anchor' < "${DOTFILES_LOCATION}"/.sops.yaml | grep -E "^${host}-system$" > /dev/null; }
fetch_sops_private_key() { bw-session get-item-field "system-nixos-$1" "sops-private"; }
fetch_bw_luks_fields() { bw-session get-item "system-nixos-$1" | jq -rc '.fields[] | select(.name | startswith("luks")) | .name'; }
bw_contains_sops_key() { bw-session get-item "system-nixos-$1" | jq -erc '.fields[] | select(.name == "sops-private")) | .name'>/dev/null; }
fetch_github_ssh_key() { bw-session get-item "system-nixos-deploy-github-ssh" | jq -re '.sshKey.privateKey'; }

unlock_bitwarden() {
  BW_SESSION="$(bw-session session "${bw_email}")"
  export BW_SESSION
  bw unlock --check > /dev/null || fatal "Vault must be unlocked"
}

# shellcheck disable=SC2030,SC2031
remote_install() {
  local host="$1"
  local bw_email="$2"
  local ssh_host="$3"
  extraArgs=()

  ! test -d "${DOTFILES_LOCATION}" && fatal "dotfiles folder not found: ${DOTFILES_LOCATION}"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  unlock_bitwarden "${bw_email}"

  post_install_files="$(mktemp -d)"
  if dotfiles_sops_contains_host "${host}"; then
    echo "Fetching sops system private key..."
    mkdir -p "$(dirname "${post_install_files}/${SOPS_AGE_SYSTEM_FILE}")"
    fetch_sops_private_key "$host" > "${post_install_files}/${SOPS_AGE_SYSTEM_FILE}"
  fi

  luks_files="$(mktemp -d)"
  fetch_bw_luks_fields "$host" | while read -r field; do
    luks_local_file="${luks_files}/$field.key"
    luks_disko_expected_file_location="/tmp/${field}.key"

    echo "Fetching luks encryption key: $field"
    bw-session get-item-field "system-nixos-${host}" "$field" > "${luks_local_file}"
    nixosAnywhereExtraArgs+=("--disk-encryption-keys" "${luks_disko_expected_file_location}" "${luks_local_file}")
  done

  nixos-anywhere --flake ".#${host}" --target-host "${ssh_host}" --extra-files "$post_install_files" "${extraArgs[@]}"

  rm -r "${luks_files}"
  rm -r "${post_install_files}"
}

disko_install() {
  local host="$1"
  local bw_email="$2"

  unlock_bitwarden "${bw_email}"

  echo "Fetching SSH deploy key due to the likelihood of private Github flakes being used"
  fetch_github_ssh_key | sudo tee /root/.ssh/ed25519
  sudo ssh-keygen -f /root/.ssh/ed25519 -y | sudo tee /root/.ssh/ed25519.pub

  # Pre-setup files
  fetch_bw_luks_fields | while read -r field; do
    echo "Fetching luks encryption key: $field"
    bw-session get-item-field "system-nixos-${host}" "$field" > "/tmp/${field}.key"
  done

  # Post setup files
  post_install_files="$(mktemp -d)"
  if bw_contains_sops_key "${host}"; then
    echo "Fetching sops system private key..."
    mkdir -p "$(dirname "${post_install_files}/${SOPS_AGE_SYSTEM_FILE}")"
    fetch_sops_private_key "$host" > "${post_install_files}/${SOPS_AGE_SYSTEM_FILE}"
  fi

  echo "Formatting disks"
  disko --mode destroy,format,mount --root-mountpoint /mnt --flake "${FLAKE_URL}#${host}"

  echo "Installing NixOS"
  sudo nixos-install --experimental-features 'nix-command flakes' --no-channel-copy --no-root-password --flake "${FLAKE_URL}"

  echo "Post Install - Copying files"
  sudo chown -R root:root "${post_install_files}/*"
  cp -r "${post_install_files}/*" /mnt

  echo "Post Install - Removing sensitive files"
  rm -rf /tmp/*.key
  rm -rf "${post_install_files}"
}

case "$1" in
  remote) remote_install "$@"     ;;
  local)  local_install "$@"      ;;
esac
