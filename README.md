[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 

This repository contains the definition of my machines using [nix](https://nixos.org/).

> [!IMPORTANT]
> **Disclaimer:** This is my personal configuration that works _for me_. I hope this helps you!
> 
> If you are new to dotfiles in general, I suggest a bare git solution to start with.

----

## Hosts

| Hostname     | CPU                   | RAM  | Primary GPU              | Secondary GPU               | OS |
|--------------|-------------------------|------|--------------------------|-------------------------------|----|
| `laptop`     | AMD Ryzen™ 7 7840HS     | 32GB | AMD Radeon™ 780M | NVIDIA® GeForce RTX™ 4060 8GB         | ❄️  |
| `work-macos` | Apple M2 Pro 8-core CPU | 16GB | Apple M2 Pro 10-core GPU |                               | 🍏  |

# Installing NixOS

## Specify a new host

1. Create a bootable USB [installer](https://nixos.org/download/):

   ```
   sudo fdisk -l
   sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```

2. On the target machine, boot onto the NixOS's installer, set `nixos`'s password using `passwd` and obtain its ip.
3. On the source machine, create a new host with the initial hardware configuration and [disko](https://github.com/nix-community/disko) set:
   ```
   HOST=new-host
   ssh nixos@<ip> -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config > hosts/$HOST/hardware-configuration.nix
   ```

## Install remotely

1. Boot onto the NixOS installer (see previous section).
2. In the source machine:
   1. Unlock a Bitwarden session:

       ```
       nix--shell -p bitwarden-cli
       bw login
       bw unlock
       export BW_SESSION="..."
       ```

   3. Run the following to install nixos remotely:

       ```
       ./bin/nixos-remote-install.sh <HOST> nixos@<IP>
       ```

5. On the target machine, once the initial installation succeeds:
 
    ```
    HOST=laptop
    BITWARDEN_EMAIL=...
    nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles/add-laptop#dotfiles-install" -- laptop $BITWARDEN_EMAIL
    ```

# Installing on Darwin

1. Install [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html).
2. Boostrap:
   ```sh
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```
   
3. Setup this dotfiles repository:
   ```sh
   HOST=work-macos
   BITWARDEN_EMAIL=...
   nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install" -- laptop $BITWARDEN_EMAIL
   ```

4. Apply:
   ```sh
   "$HOME"/.dotfiles/overlays/dotfiles/dotfiles.sh sync
   ```

6. Reboot!

# Secrets

Initialize:
1. Generate key pair using `age-keygen`.
2. Export the public key to `.sops.yaml` and the private key to `$HOME/.config/sops/age/keys.txt`.
3. Set the `path_regex` of the files in `.sops.yaml` and update `.gitattributes` accordingly.
4. Create an empty `git-secrets` folder under the target `host`:
5. Initialize the `git-filter`:
   ```sh
   ./bin/sops-git-filter.sh init {host}
   ```
6. Add non-sensitive secrets to test the setup:
   1. `sops-nix` uses `sops.yaml` and `default.nix` as detailed in their [docs](https://github.com/Mic92/sops-nix).
   2. `git-filter` uses filter(s) added to `.gitattributes`.

Import:
1. Add bitwarden-cli to `$PATH`: 
   ```sh
   nix-shell -p bitwarden-cli
   ```
2. Login, unlock and set `BW_SESSION`: 
   ```sh
   bw login && bw unlock
   ```
3. Run the following to export the private key:
   ```su
   HOST=HELLO
   bw get item "sops-age-key-${HOST}" | jq --raw-output '.fields[] | select(.name=="private") | .value' >> "$HOME/.config/sops/age/keys.txt"
   ```
4. Actually apply the `git-filter`: 
   ```su
   git rm hosts/$HOST/secrets && git checkout HEAD hosts/$HOST/secrets
   ```
5. Test by trying to read the existing secrets.

