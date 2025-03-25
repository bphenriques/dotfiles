[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 

This repository contains the definition of my machines using [nix](https://nixos.org/).

> [!IMPORTANT]
> **Disclaimer:** This is my personal configuration that works _for me_. I hope this helps you!
> 
> If you are new to dotfiles in general, I suggest a bare git solution to start with.

----

# Hosts

| Hostname     | CPU                     | RAM  | Integrated GPU           | Discreet GPU                  | OS |
|--------------|-------------------------|------|--------------------------|-------------------------------|----|
| `laptop`     | AMD Ryzen™ 7 7840HS     | 32GB | AMD Radeon™ 780M         | NVIDIA® GeForce RTX™ 4060 8GB | ❄️ |
| `work-macos` | Apple M2 Pro 8-core CPU | 16GB | Apple M2 Pro 10-core GPU |                               | 🍏 |

Feel free to take a look around and ask me anything!

# Install

### NixOS

#### Create host

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
   ssh nixos@$TARGET_IP -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config > hosts/$HOST/hardware-configuration.nix
   ```

4. Create a new host directory with the hardware configuration and basic nixos settings.

#### Install remotely

1. Boot onto the NixOS installer (see previous section).
2. On a NixOS source machine run:
    ```shell
    HOST=laptop
    TARGET_IP=192.168.68.58
    nix run ./bin/nixos-remote-install.sh $HOST nixos@$TARGET_IP
    ```

3. On the target machine, run the post-install:
    ```shell
    HOST=laptop
    BITWARDEN_EMAIL=me@me.com
    nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#post-install" -- $HOST $BITWARDEN_EMAIL
    ```

### Darwin

1. Install [`nix` (Determinate Systems)](https://determinate.systems/nix-installer/).
2. Boostrap:
   ```shell
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```
   
3. Run the post-install:
   ```shell
   HOST=work-macos
   BITWARDEN_EMAIL=me@me.com
   nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#post-install" -- $HOST $BITWARDEN_EMAIL
   ```

5. Apply:
   ```shell
   nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles" -- sync
   ```

6. Reboot!
