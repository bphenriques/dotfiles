#shellcheck shell=bash

set -e

# FIXME
BRANCH_NAME="${BRANCH_NAME:-wayland-move-btrfs}"
FLAKE_URL="${FLAKE_URL:-github:bphenriques/dotfiles/${BRANCH_NAME}}"
SOPS_AGE_SYSTEM_FILE="/var/lib/sops-nix/system-keys.txt"

fatal() { printf '[FAIL] %s\n' "$1" 1>&2; exit 1; }
info() { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }

unlock_bitwarden() {
  info "${host} - Unlocking Bitwarden account (${bw_email})..."
  BW_SESSION="$(bw-session session "$1")"
  export BW_SESSION
}

remote_install() {
  local host="$1"
  local bw_email="$2"
  local ssh_host="$3"
  extraArgs=()

  ! test -d "${DOTFILES_LOCATION}" && fatal "dotfiles folder not found: ${DOTFILES_LOCATION}"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  unlock_bitwarden "$bw_email"

  info "Checking for sops keys..."
  post_format_files="$(mktemp -d)"  
  if dotfiles-secrets fetch sops-secret "${host}" > /dev/null; then
    info "Fetching host sops key..."
    mkdir -p "$(dirname "${post_format_files}/${SOPS_AGE_SYSTEM_FILE}")"
    dotfiles-secrets fetch sops-secret "$host" > "${post_format_files}/${SOPS_AGE_SYSTEM_FILE}"
  fi

  info "Checking for luks encryption keys..."
  luks_files="$(mktemp -d)"
  if dotfiles-secrets fetch luks-key "$host" >/dev/null; then
    luks_local_file="${luks_files}/luks-interactive-password.key"
    luks_disko_expected_file_location="/tmp/luks-interactive-password.key"

    info "Fetching luks encryption key..."
    dotfiles-secrets fetch luks-key "$host" > "${luks_local_file}"
    nixosAnywhereExtraArgs+=("--disk-encryption-keys" "${luks_disko_expected_file_location}" "${luks_local_file}")
  fi

  nixos-anywhere --flake "${FLAKE_URL}#${host}" --target-host "${ssh_host}" --extra-files "$post_format_files" "${extraArgs[@]}"

  rm -r "${luks_files}"
  rm -r "${post_format_files}"
}

local_install() {
  local host="$1"
  local bw_email="$2"

  unlock_bitwarden "$bw_email"

  info "Fetching SSH deploy key due to private Github flakes..."
  sudo mkdir -m 700 -p /root/.ssh
  dotfiles-secrets fetch ssh-private-key "${host}" | sudo tee /tmp/github-deploy-ssh >/dev/null
  sudo chmod 700 /tmp/github-deploy-ssh
  sudo cp /tmp/github-deploy-ssh /root/.ssh/id_ed25519
  sudo ssh-keygen -f /root/.ssh/id_ed25519 -y | sudo tee /root/.ssh/id_ed25519.pub >/dev/null

  info "Fetching luks encryption key..."
  dotfiles-secrets fetch luks-key "$host" > "/tmp/luks-interactive-password.key" || fatal "No luks key available"

  info "Formatting disks..."
  sudo disko --mode destroy,format,mount --root-mountpoint /mnt --flake "${FLAKE_URL}#${host}"

  info "Checking for sops keys..."
  if dotfiles-secrets fetch sops-secret "$host" > /dev/null; then
    info "Copying sops system private key...: "/mnt/${SOPS_AGE_SYSTEM_FILE}""
    sudo mkdir -p "$(dirname "/mnt/${SOPS_AGE_SYSTEM_FILE}")"
    dotfiles-secrets fetch sops-secret "$host" | sudo tee "/mnt/${SOPS_AGE_SYSTEM_FILE}" > /dev/null
    sudo chown -R root:root "/mnt/${SOPS_AGE_SYSTEM_FILE}"
  fi

  info "Installing NixOS..."
  sudo nixos-install --no-write-lock-file --no-channel-copy --no-root-password --flake "${FLAKE_URL}#${host}"

  info "Cleaning up LUKS keys..."
  sudo rm -rf /tmp/*.key

  success 'Done! Press any key to reboot. Press F12 if you need to select the right drive to boot'; read -r _;
  reboot
}

case "$1" in
  remote) shift 1 && remote_install "$@"     ;;
  local)  shift 1 && local_install "$@"      ;;
esac
