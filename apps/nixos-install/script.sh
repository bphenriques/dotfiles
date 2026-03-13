#shellcheck shell=bash
set -euo pipefail

FLAKE_URL="${FLAKE_URL:-github:bphenriques/dotfiles/${BRANCH_NAME:-main}}"
DOTFILES_LOCATION="${DOTFILES_LOCATION:-$HOME/.dotfiles}"
SOPS_AGE_SYSTEM_FILE="/var/lib/sops-nix/system-keys.txt"

fatal()   { printf '[FAIL] %s\n' "$1" >&2; exit 1; }
info()    { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }

usage() {
  cat <<EOF
Install NixOS on a new machine.

Usage:
  $0 remote <host> <bw-email> <ssh-host>   Install remotely via nixos-anywhere
  $0 local  <host> <bw-email>              Install locally (booted from USB)
  $0 rescue <host> <bw-email>              Mount encrypted disks for recovery

Prerequisites:
  1. Create host config: hosts/<host>/{config.nix,hardware-configuration.nix,disko.nix}
  2. Set up secrets: dotfiles-secrets init-host <host> [--luks]
  3. Create hosts/<host>/.sops.yaml with the SOPS public key

Examples:
  $0 remote laptop me@me.com nixos@192.168.1.50
  $0 remote homelab/compute me@me.com nixos@192.168.1.100
  $0 local laptop me@me.com
EOF
  exit 1
}

unlock_bitwarden() {
  local bw_email="$1"
  info "Unlocking Bitwarden account (${bw_email})..."
  BW_SESSION="$(bw-session session "$bw_email")"
  export BW_SESSION
}

remote_install() {
  [[ $# -eq 3 ]] || usage
  local host="$1"
  local bw_email="$2"
  local ssh_host="$3"
  local -a extraArgs=()

  # Pre-flight check: requires local clone (remote runs use FLAKE_URL for actual install)
  ! test -d "${DOTFILES_LOCATION}" && fatal "dotfiles folder not found: ${DOTFILES_LOCATION}"
  ! test -d "${DOTFILES_LOCATION}/hosts/${host}" && fatal "No matching '${host}' under '${DOTFILES_LOCATION}/hosts'"

  unlock_bitwarden "$bw_email"

  info "Fetching sops key for ${host}..."
  post_format_files="$(mktemp -d)"
  if dotfiles-secrets "$bw_email" fetch sops-secret "${host}" > /dev/null 2>&1; then
    mkdir -p "$(dirname "${post_format_files}/${SOPS_AGE_SYSTEM_FILE}")"
    dotfiles-secrets "$bw_email" fetch sops-secret "${host}" > "${post_format_files}/${SOPS_AGE_SYSTEM_FILE}"
    success "Host sops key fetched"
  else
    fatal "Host sops key not found in Bitwarden (item: sops-secret/${host})"
  fi

  info "Checking for luks encryption keys..."
  luks_files="$(mktemp -d)"
  if dotfiles-secrets "$bw_email" fetch luks-key "$host" >/dev/null; then
    luks_local_file="${luks_files}/luks-interactive-password.key"
    luks_disko_expected_file_location="/tmp/luks-interactive-password.key"

    info "Fetching luks encryption key..."
    dotfiles-secrets "$bw_email" fetch luks-key "$host" > "${luks_local_file}"
    extraArgs+=("--disk-encryption-keys" "${luks_disko_expected_file_location}" "${luks_local_file}")
  fi

  nixos-anywhere --flake "${FLAKE_URL}#${host}" --target-host "${ssh_host}" --ssh-option "IdentitiesOnly=yes" --extra-files "$post_format_files" "${extraArgs[@]}"

  rm -r "${luks_files}"
  rm -r "${post_format_files}"
}

local_install() {
  [[ $# -eq 2 ]] || usage
  local host="$1"
  local bw_email="$2"

  trap 'sudo rm -f /tmp/luks-interactive-password.key' EXIT

  unlock_bitwarden "$bw_email"

  info "Fetching SSH deploy key due to private Github flakes..."
  local tmp_ssh_key="/tmp/github-deploy-ssh.$$"
  sudo mkdir -m 700 -p /root/.ssh
  dotfiles-secrets "$bw_email" fetch ssh-private-key "${host}" | sudo tee "$tmp_ssh_key" >/dev/null
  sudo chmod 600 "$tmp_ssh_key"
  sudo install -m 600 "$tmp_ssh_key" /root/.ssh/id_ed25519
  sudo ssh-keygen -f /root/.ssh/id_ed25519 -y | sudo tee /root/.ssh/id_ed25519.pub >/dev/null
  sudo rm -f "$tmp_ssh_key"

  if dotfiles-secrets "$bw_email" exists luks-key "$host"; then
    info "Fetching luks encryption key..."
    dotfiles-secrets "$bw_email" fetch luks-key "$host" > "/tmp/luks-interactive-password.key"
  fi

  info "Formatting disks..."
  sudo disko --mode destroy,format,mount --root-mountpoint /mnt --flake "${FLAKE_URL}#${host}"

  info "Fetching sops key for ${host}..."
  if dotfiles-secrets "$bw_email" fetch sops-secret "${host}" > /dev/null 2>&1; then
    info "Copying sops system private key to /mnt/${SOPS_AGE_SYSTEM_FILE}"
    sudo mkdir -p "$(dirname "/mnt/${SOPS_AGE_SYSTEM_FILE}")"
    dotfiles-secrets "$bw_email" fetch sops-secret "${host}" | sudo tee "/mnt/${SOPS_AGE_SYSTEM_FILE}" > /dev/null
    sudo chown root:root "/mnt/${SOPS_AGE_SYSTEM_FILE}"
    sudo chmod 600 "/mnt/${SOPS_AGE_SYSTEM_FILE}"
    success "Host sops key installed"
  else
    fatal "Host sops key not found in Bitwarden (item: sops-secret/${host})"
  fi

  info "Installing NixOS..."
  sudo nixos-install --no-write-lock-file --no-channel-copy --no-root-password --flake "${FLAKE_URL}#${host}"

  success 'Done! Press any key to reboot. Press F12 if you need to select the right drive to boot'; read -r _;
  reboot
}

rescue() {
  [[ $# -eq 2 ]] || usage
  local host="$1"
  local bw_email="$2"

  unlock_bitwarden "$bw_email"

  dotfiles-secrets "$bw_email" fetch luks-key "$host" > "/tmp/luks-interactive-password.key" || fatal "No luks key available"
  sudo disko --mode mount --root-mountpoint /mnt --flake "${FLAKE_URL}#${host}"
}

[[ $# -lt 1 ]] && usage

case "$1" in
  remote) shift; remote_install "$@" ;;
  local)  shift; local_install "$@"  ;;
  rescue) shift; rescue "$@"         ;;
  *) usage ;;
esac
