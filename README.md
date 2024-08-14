[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 

This repository contains the definition of my machines using [nix](https://nixos.org/).

> [!IMPORTANT]
> **Disclaimer:** This is my personal configuration that works _for me_. I hope this helps you!
> 
> If you are new to dotfiles in general, I suggest a bare git solution to start with.

----

| Hostname     | CPU                   | RAM  | Primary GPU              | Secondary GPU               | OS |
|--------------|-------------------------|------|--------------------------|-------------------------------|----|
| `laptop`     | AMD Ryzen™ 7 7840HS     | 32GB | AMD Radeon™ 780M | NVIDIA® GeForce RTX™ 4060 8GB         | ❄️  |
| `work-macos` | Apple M2 Pro 8-core CPU | 16GB | Apple M2 Pro 10-core GPU |                               | 🍏  |

# Installing NixOS

## Create new host

1. Create a bootable USB [installer](https://nixos.org/download/):

   ```
   sudo fdisk -l
   sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```

2. On the target machine, boot onto the NixOS's installer, set `nixos`'s password using `passwd` and fetch its ip.
3. On the source machine, seed the host settings:
   ```
   HOST=new-host
   mkdir hosts/$HOST
   ssh nixos@<ip> -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config > hosts/$HOST/hardware-configuration.nix
   ```

4. Set its disk's layout using [disko](https://github.com/nix-community/disko), base configuration and add it to the `nixosConfigurations` block under `flake.nix`.

## Install remotely

1. Boot onto the NixOS installer (see previous section).
2. In the source machine run the following (replace `<HOST>` and `<IP>`). The script automatically generates a SSH key and retrieves credentials from my secret vault:

    ```
    bw login
    bw unlock
    ./bin/nixos-remote-install.sh <HOST> nixos@<IP>
    ```

5. Once the initial installation succeeds, run the dotfiles installer as follows:
    ```
    $ nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install" -- laptop
    ```

# Installing on Darwin

1. Install [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html).
2. Boostrap:
   ```sh
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```
   
3. Setup this dotfiles repository. Replace `{host}` with the intended darwin host listed under `hosts`:
   ```sh
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#dotfiles-install -- $HOST
   ```

4. Apply:
   ```sh
   "$HOME"/.dotfiles/overlays/dotfiles/dotfiles.sh sync
   ```

5. Import the GPG Key using `gpg --import`. You may need to restart.
6. Reboot!

# Secrets

Initialize:
1. Generate key pair:
   ```sh
   nix-shell -p age --command 'age-keygen'
   ```
2. Export the public key to `.sops.yaml` and the private key to `$HOME/.config/sops/age/keys.txt`.
3. Set the `path_regex` of the files in `.sops.yaml` and update `.gitattributes` accordingly.
4. Create an empty `secrets` folder under the target `host`:
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

