set -e

FLAKE_URL="github:bphenriques/dotfiles/wayland-move-btrfs#laptop"

press_to_continue() { echo 'Press any key to continue'; read -r _; }

EMAIL_ADDRESS="$1"
if [ -z "${EMAIL_ADDRESS}" ];
  echo "EMAIL_ADDRESS not set as first argument."
  exit 1
fi

echo "Generating root ssh key as it is required to run disko due to private Github flakes being used"
sudo ssh-keygen -t ed25519 -C "${EMAIL_ADDRESS}"
echo "Copy the following public key to: https://github.com/settings/ssh/new"
press_to_continue

echo "Fetching up secrets from Bitwarden..."
BW_SESSION="$(bw-session session "${EMAIL_ADDRESS}")"
export BW_SESSION

echo "\tLUKS keys..."
bw-session get-item-field "system-nixos-laptop" luks-main    > "/tmp/luks-main.key"
bw-session get-item-field "system-nixos-laptop" luks-backup  > "/tmp/luks-backup.key"

echo "\tSOPS private key..."
bw-session get-item-field "sops-age-key-laptop-system" "private" > "/tmp/system-keys.txt"

echo "Will now destroy, format and mount the disks as specified under the flake and then install nixos: ${FLAKE_URL}."
press_to_continue

sudo nix --experimental-features 'nix-command flakes' run 'github:nix-community/disko/v1.11.0' -- \
  --mode destroy,format,mount \
  --root-mountpoint /mnt \
  --flake "${FLAKE_URL}"
sudo nixos-install --experimental-features 'nix-command flakes' --no-channel-copy --no-root-password --flake "${FLAKE_URL}"

echo "Copying important files"
mkdir -p "/mnt/var/lib/sops-nix/"
cp /tmp/system-keys.txt "/mnt/var/lib/sops-nix/system-keys.txt"
sudo chown -R root:root /mnt/var/lib/sops-nix

rm "/tmp/system-keys.txt" "/tmp/luks-main.key" "/tmp/luks-backup.key"
