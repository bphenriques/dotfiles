# NixOS Installation

## Prerequisites

1. Create bootable USB from [NixOS installer](https://nixos.org/download/) 
   ```shell
   sudo fdisk -l
   sudo dd bs=4M if=<ISO_FILE> of=<DRIVE> status=progress oflag=sync
   ```

2. Run and extract the hardware configuration:
   ```
   nixos-generate-config --no-filesystems --root /mnt --show-hardware-config
   ```
3. Create host directory: `hosts/<name>/` with `config.nix`, `hardware-configuration.nix`, `disko.nix`
4. Bootstrap secrets in Bitwarden: `dotfiles-secrets init-host <BITWARDEN_EMAIL> <host> [--luks]`
5. Update `.sops.yaml` with the public key from step 2

## Install locally

1. Boot onto the NixOS installer (see previous section).
2. On the target machine, run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#nixos-install -- local "$HOST" "$BITWARDEN_EMAIL"   
    ```

3. (if workstation) Once installed and booted onto the NixOS installation run:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#desktop-post-install -- "$HOST" "$BITWARDEN_EMAIL"
    ```

## Install remotely

1. Boot onto the NixOS installer (see previous section). Set `nixos`'s password using `passwd` and get its IP (`ip addr`). You likely already have added a static IP. 
2. On a NixOS source machine run:
    ```shell
    export FLAKE_URL="github:bphenriques/dotfiles/$(git branch --show-current)"
    HOST=compute
    TARGET_IP=192.168.1.196
    BITWARDEN_EMAIL="$(cat /tmp/email)"
    nix run $FLAKE_URL#nixos-install --refresh -- remote "$HOST" "$BITWARDEN_EMAIL" "nixos@$TARGET_IP"
    ```

3. (if workstation) Once the new machine reboots, run the post-installation setup:
    ```shell
    export FLAKE_URL="github:bphenriques/dotfiles/$(git branch --show-current)"
    HOST=laptop
    BITWARDEN_EMAIL="$(cat /tmp/email)"
    nix run --no-write-lock-file --extra-experimental-features 'nix-command flakes' $FLAKE_URL#desktop-post-install --refresh -- "$HOST" "$BITWARDEN_EMAIL"
    ```

## Install SD card

1. Identify the SD card device: `sudo fdisk -l`
2. Build, flash, and provision secrets:
   ```shell
   HOST=inky
   DEVICE=/dev/sdX
   BITWARDEN_EMAIL=me@me.com
   export FLAKE_URL=.  # Use local checkout (default: github:bphenriques/dotfiles/main)
   nix run .#nixos-install -- sd-card "$HOST" "$BITWARDEN_EMAIL" "$DEVICE"
   ```

# Misc

## NVMes

Brand new NVMes might need some adjustments as they might have conservative settings, specifically 
[report their logical block address size as 512 bytes, even though they use larger blocks physically - typically 4 KiB, 8 KiB, or sometimes larger.](https://wiki.archlinux.org/title/Advanced_Format#NVMe_solid_state_drives)

To start, install `nvme-cli`:
```
$ nix-shell -p nvme-cli
```

Following this:
1. List the nvmes available: `sudo nvme list`
2. Then, check the NVMe LBA format to see if it can be optimised:

   ```
   $ sudo nvme id-ns -H /dev/nvme0n1 | grep "Relative Performance"
   LBA Format  0 : Metadata Size: 0   bytes - Data Size: 512 bytes - Relative Performance: 0x2 Good
   LBA Format  1 : Metadata Size: 0   bytes - Data Size: 4096 bytes - Relative Performance: 0x1 Better (in use)
   ```
   
3. In this case, the best option is "in use". Otherwise, I recommend changing the logical block size address:
   ```
   $ nvme format --lbaf=1 /dev/nvme0n1
   ```

   Where `--lbaf` corresponds to the number next to "LBA Format". In the above example is set to `1`.

Source
- https://wiki.archlinux.org/title/Advanced_Format#NVMe_solid_state_drives

