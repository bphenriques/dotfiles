# Setting up new host

1. Create a bootable USB [NixOS installer](https://nixos.org/download/).
   ```shell
   sudo fdisk -l
   sudo dd bs=4M if=<ISO_FILE> of=<DRIVE> status=progress oflag=sync
   ```

2. If you are still new to NixOS install as it is, extract its configuration and play around.
3. Otherwise, if you have a host set, extract its `hardware-configuration.nix` by running:

   ```
   $ nixos-generate-config --no-filesystems --root /mnt --show-hardware-config
   ```

4. Finally, create a new `host/<name>` and start small with `config.nix`, `hardware-configuration.nix`, and `disko.nix` ([examples](https://github.com/nix-community/disko/tree/master/example)).

This is what I consider a minimal _sane_ installation (e.g., tried [impermanence](https://github.com/nix-community/impermanence) but it is not for me).

Note: `nixos-facter`](https://github.com/nix-community/nixos-facter) is also interesting, but seems a bit opaque.

# Installation

For the following to work, we need to prepare some base credentials:
- SOPS
- Luke
- SSH
- GPG

# Install remotely

1. Boot onto the NixOS installer (see previous section). Set `nixos`'s password using `passwd` and get its IP. 
2. On a NixOS source machine run:
    ```shell
    HOST=laptop
    TARGET_IP=192.168.68.58
    BITWARDEN_EMAIL=me@me.com
    nix run github:bphenriques/dotfiles#nixos-install -- remote "$HOST" "$BITWARDEN_EMAIL" "nixos@$TARGET_IP"
    ```

3. Once the new machine reboots, run the post-installation setup:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#post-install -- "$HOST" "$BITWARDEN_EMAIL"
    ```

# Install locally

1. Boot onto the NixOS installer (see previous section).

2. On the target machine, run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#nixos-install -- local "$HOST" "$BITWARDEN_EMAIL"   
    ```

3. Once installed and booted onto the NixOS installation run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#post-install -- "$HOST" "$BITWARDEN_EMAIL"
    ```


## Hardware

### NVMes


Found out that new NVME might need fine-tuning:
1. List the nvmes available: `sudo nvme list`
2. Then, list the blocks of the device: ``lsblk -t /dev/nvme0n1`
3. You check if it supports
# in the official docs.512 (physical/logical): `lsblk -t /dev/nvme0n1`
# But.. it can support 4096/4096: `sudo nvme id-ns -H /dev/nvme031` (difference between "in-use" an in the official docs.512/512 is for compatibility and we can increase it: `sudo nvme format --lbaf=1 /dev/nvme0n1`.
# - Where lbaf corresponds to the number next to "LBA Format"
#
# Now let's check again which LBA Format is in-use: `sudo nvme id-ns -H /dev/nvme0n1`
#
# Source
- https://wiki.archlinux.o in the official docs.ormat
- https://www.high-availability.com/docs/ZFS-Tuning-Guide/#alignment-shift-ashiftn

