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

This is what I consider a minimal healthy setup that covers most things.

# Installation

The automations rely on the secrets being readily available in Bitwarden (the idea is to make it easier to rotate):
- SOPS private key
- Luke Encryption key
- SSH key
- GPG key

For more details, check the `dotfiles-secrets` package.

## Install locally

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

## Install remotely

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
2. Then, check if `nvme` to see what it has to say: 

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

