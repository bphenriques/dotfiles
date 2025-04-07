# Create host

1. Create a bootable USB [installer](https://nixos.org/download/):
   ```shell
   sudo fdisk -l
   sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```

2. On the target machine, boot onto the NixOS's installer, set `nixos`'s password using `passwd` and obtain its IP.
3. On the source machine, create a new host with the initial hardware configuration and [disko](https://github.com/nix-community/disko) set:
   ```shell
   HOST=laptop
   TARGET_IP=192.168.68.58
   ssh "nixos@$TARGET_IP" -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config > "hosts/$HOST/hardware-configuration.nix"
   ```

4. Create a new host directory with the hardware configuration and basic nixos settings.

# Install remotely

1. Boot onto the NixOS installer (see previous section).
2. On a NixOS source machine run:
    ```shell
    HOST=laptop
    TARGET_IP=192.168.68.58
    BITWARDEN_EMAIL=me@me.com
    nix run .#nixos-install -- remote "$HOST" "$BITWARDEN_EMAIL" "nixos@$TARGET_IP"
    ```

3. On the target machine, run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#post-install" -- "$HOST" "$BITWARDEN_EMAIL"
    ```

# Install locally

1. Boot onto the NixOS installer (see previous section).

2. On the target machine, run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles/wayland-move-btrfs#nixos-install" -- local "$HOST" "$BITWARDEN_EMAIL"   
    ```

3. Once installed and booted onto the NixOS installation run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles/wayland-move-btrfs#post-install" -- "$HOST" "$BITWARDEN_EMAIL"
    ```

