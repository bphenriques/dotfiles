# Run:
# Run NixOS installer
# Install flakes in the system
# nix profile install github:bphenriques/dotfiles/wayland-move-btrfs#bw-session
# nix profile install nixpkgs#jq

set -e

BW_SESSION="$(bw-session session "$1")"
export BW_SESSION

# Authentication - I have private flakes, therefore need to set the Github token
echo "Fetching Github credentials..."
export GITHUB_TOKEN="$(bw-session get-item-field "Github Token" "token")"
export NIX_CONFIG="access-tokens = github.com=$GITHUB_TOKEN"

# Has to match disko settings
echo "Fetching Luks keys..."
bw-session get-item-field "system-nixos-laptop" luks-main    > "/tmp/luks-main.key"
bw-session get-item-field "system-nixos-laptop" luks-backup  > "/tmp/luks-backup.key"

# The key has to match the public key under .sops.yaml
echo "Fetching Sops private keys..."
bw-session get-item-field "sops-age-key-laptop-system" "private" > "/tmp/system-keys.txt"

echo "Running Disko..."
sudo nix run 'github:nix-community/disko/latest#disko-install' -- \
  --flake "github:bphenriques/dotfiles/wayland-move-btrfs#laptop" \
  --disk vda /dev/disk/by-path/pci-0000:05:00.0-nvme-1 \
  --extra-files /tmp/system-keys.txt "/var/lib/sops-nix/system-keys.txt"

# sudo nix run 'github:nix-community/disko/latest#disko-install' -- \
#  --flake '/tmp/config/etc/nixos#mymachine' \
#  --disk vda /dev/disk/by-path/pci-0000:05:00.0-nvme-1 \
#  --write-efi-boot-entries

# TODO: delete dangling files afterwards

