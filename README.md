[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 

This repository contains the definition of my machines using [nix](https://nixos.org/).

> [!IMPORTANT]
> **Disclaimer:** This is my personal configuration that works _for me_. I hope this helps you!
> 
> If you are new to dotfiles in general, I suggest a bare git solution to start with.

----

# Hosts

| Hostname     | CPU                     | RAM  | Primary GPU              | Secondary GPU                 | OS |
|--------------|-------------------------|------|--------------------------|-------------------------------|----|
| `laptop`     | AMD Ryzen™ 7 7840HS     | 32GB | AMD Radeon™ 780M         | NVIDIA® GeForce RTX™ 4060 8GB | ❄️ |
| `work-macos` | Apple M2 Pro 8-core CPU | 16GB | Apple M2 Pro 10-core GPU |                               | 🍏 |

# Flake Outputs

Listing only the most relevant outputs. See the source-code for more details.

### Packages

- `dotfiles`: Wrapper around `nix`/`nixos-rebuild` operations.
- `project`: Custom [`fzf`](https://github.com/junegunn/fzf) integration to quickly open projects within `PROJ_DIR` or `XDG_DOCUMENTS_DIR`. Includes `fish` widget.
- `fzf-rg`: [`fzf`](https://github.com/junegunn/fzf) + [`ripgrep`](https://github.com/BurntSushi/ripgrep). Includes `fish` widget.
- `fzf-fd`: [`fzf`](https://github.com/junegunn/fzf) + [`fd`](https://github.com/sharkdp/fd). Includes `fish` widget.
- `preview`: Custom _barebones_ terminal file previewer to together with the other widgets. I really like [`yazi`](https://yazi-rs.github.io/) previewer but can't use it in isolation.

### [Home Manager](https://github.com/nix-community/home-manager) modules

- `programs-dotfiles`: Accompanies the `dotfiles` package.
- `programs-project`: Accompanies the `project` package.
- `programs-fzf-fd`: Accompanies the `fzf-fd` package.
- `programs-fzf-rg`: Accompanies the `fzf-rg` package.
- `xdg-mime-apps`: Custom `xdg-mime-apps` module to abstract setting the common application for typical common text/images/audio/video mimes.

### NixOS modules

- `proton-run`: [`proton-ge-custom`](https://github.com/GloriousEggroll/proton-ge-custom) runner using a global prefix for ad-hoc executions.
- `services-input-solaar`: [`Solaar`]([https://github.com/GloriousEggroll/proton-ge-custom](https://github.com/pwr-Solaar/Solaar)) service.
- `boot-theme`: Custom boot configuration including [plymouth-themes](https://github.com/adi1090x/plymouth-themes), settings to hide logs, and an OLED friendly Grub2 theme.

### [Nix Darwin](https://github.com/LnL7/nix-darwin) modules

- `system-screencapture`: Ensures the the screencapture directory exists.
- `system-desktop`: Sets a wallpaper on all desktops. Not perfect but good enough for me.

# Install

### NixOS

#### Create host

1. Create a bootable USB [installer](https://nixos.org/download/):
   ```sh
   sudo fdisk -l
   sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```

2. On the target machine, boot onto the NixOS's installer, set `nixos`'s password using `passwd` and obtain its IP.
3. On the source machine, create a new host with the initial hardware configuration and [disko](https://github.com/nix-community/disko) set:
   ```sh
   HOST=new-host
   TARGET_IP=192.168.68.58
   ssh nixos@$TARGET_IP -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config > hosts/$HOST/hardware-configuration.nix
   ```

#### Install remotely

1. Boot onto the NixOS installer (see previous section).
2. On a NixOS source machine run:
    ```sh
    HOST=new-host
    TARGET_IP=192.168.68.58
    ./bin/nixos-remote-install.sh $HOST nixos@$TARGET_IP
    ```

3. On the target machine, run the following after the installation:
    ```sh
    HOST=laptop
    BITWARDEN_EMAIL=...
    nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install" -- $HOST $BITWARDEN_EMAIL
    ```

### Darwin

1. Install [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html).
2. Boostrap:
   ```sh
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```
   
3. Setup the dotfiles repository:
   ```sh
   HOST=work-macos
   BITWARDEN_EMAIL=...
   nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install" -- laptop $BITWARDEN_EMAIL
   ```

5. Apply:
   ```sh
   "$HOME"/.dotfiles/packages/dotfiles/dotfiles.sh sync
   ```

6. Reboot!

# Secrets

- Secrets: [`sops-nix`](https://github.com/Mic92/sops-nix).
- Private information: private flake under `bphenriques/dotfiles-private`.

Adding new hosts requires:
1. Generate key pair using: `nix-shell -p age --command "age-keygen"`.
2. Export the private key to `$HOME/.config/sops/age/keys.txt` and upload to Bitwarden using the format `sops-age-key-$HOST-$USER` with a `private` field inside.
3. Add new host to `.sops.yaml` using the public key to `.sops.yaml` and the correct `path_regex`.
