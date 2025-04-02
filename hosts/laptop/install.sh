# Run:
# nix-shell -p https://github.com/bphenriques/dotfiles#bw-session

BRANCH_NAME="${BRANCH_NAME:-main}"
FLAKE_URL="${FLAKE_URL:-github:bphenriques/dotfiles/${BRANCH_NAME}}"
SOPS_AGE_SYSTEM_FILE="/var/lib/sops-nix/system-keys.txt"

info() { printf '[ \033[00;34m  \033[0m ] %s\n' "$1"; }
error() { printf '[\033[0;31mERROR\033[0m] %s\n' "$1" 1>&2; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }

check_bitwarden_unlocked() {
  if [ -z "$BW_SESSION" ]; then
    BW_SESSION="$(bw-session session "${bw_email}")"
    export BW_SESSION
  fi

  bw unlock --check > /dev/null || fatal "Vault must be unlocked"
}

# Authentication - I have private flakes, therefore need to set the Github token
info "Fetching Github credentials..."
export GITBUB_TOKEN="$(nix run .#bw-session -- get-item-field "Github Token" "token")"
export NIX_CONFIG="access-tokens = github.com=$GITHUB_TOKEN"

# Has to respect disko.nix
info "Fetching Luks keys..."
nix run .#bw-session -- get-item-field "system-nixos-laptop" luks-main-key    > "/tmp/luks-main.key"
nix run .#bw-session -- get-item-field "system-nixos-laptop" luks-backup-key  > "/tmp/luks-backup.key"

# Sops
info "Fetching Sops private keys..."
bw-session get-item-field "system-nixos-laptop" "sops-system-private-key" > "/tmp/system-keys.txt"

info "Running Disko..."
sudo nix run 'github:nix-community/disko/latest#disko-install' -- \
  --flake "${FLAKE_URL}#${host}" \
  --disk "${disk_name}" "${disk_device}" \
  --extra-files /tmp/system-keys.txt "${SOPS_AGE_SYSTEM_FILE}"
  --dry-run

