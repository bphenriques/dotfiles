[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 

This repository contains the definition of my machines using [nix](https://nixos.org/).

> [!IMPORTANT]
> **Disclaimer:** This is my personal configuration that works _for me_. I hope this helps you!
> 
> If you are new to dotfiles in general, I suggest a bare git solution to start with.

----

What you will find here:
- [nixos-anywhere](https://github.com/nix-community/nixos-anywhere) to automate the installation remotely.
- [disko](https://github.com/nix-community/disko) to declaratively format my machines.
- [sops-nix](https://github.com/Mic92/sops-nix) for critical secrets that I do not want in the nix store.
- Combination of `git-filter` and [sops](https://github.com/getsops/sops) for non-critical sensitive information required in Nix evaluation time that I do not mind being in plain-text in the nix store.

  | Hostname     | CPU                   | RAM  | Primary GPU              | Secondary GPU               | OS |
  |--------------|-------------------------|------|--------------------------|-------------------------------|----|
  | `laptop`     | AMD Ryzen™ 7 7840HS     | 32GB | AMD Radeon™ 780M | NVIDIA® GeForce RTX™ 4060 8GB         | ❄️  |
  | `work-macos` | Apple M2 Pro 8-core CPU | 16GB | Apple M2 Pro 10-core GPU |                               | 🍏  |

# Installing NixOS

## Locally

1. Create a bootable USB [installer](https://nixos.org/download/):

   ```
   $ sudo fdisk -l
   $ sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```
   
2. TODO

## Remotely

1. On the source machine, create a bootable USB [installer](https://nixos.org/download/):

   ```
   $ sudo fdisk -l
   $ sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```

2. On the target machine, boot onto the NixOS's installer, set `nixos`'s password using `passwd` and gets its local ip using `ifconfig`.

3. (skip if done): On the source machine, generate a new configuration:

   1. Generate the host specific settings:
   ```
   HOST=new-host
   mkdir $HOST
   ssh nixos@<ip> -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config > hosts/$HOST/hardware-configuration.nix
   ```

   2. Duplicate one of the NixOS hosts configuration folder and add an entry in `flake.nix`.
   
   3. Update `disk-config.nix` ([disko](https://github.com/nix-community/disko)) considering the disks available:
   ```
   ssh nixos@<ip> -- lsblk -p
   ```

4. In the source machine run the following (replace `<HOST>` and `<IP>`). The script automatically generates a SSH key and retrieves credentials from my secret vault:

    ```
    bw login
    bw unlock
    $ ./bin/nixos-remote-install.sh laptop nixos@192.168.68.59 --sops-age-destination /persist/data/bphenriques/home/bphenriques/.config/sops/age/keys.txt
    ```

5. Once the initial installation succeeds, run the dotfiles installer as follows:
    ```
    $ nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install" -- laptop
    ```

# Installing on Darwin

1. Ensure [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html) is installed.
2. Boostrap:
   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```
   
3. Setup this dotfiles repository. Replace `{host}` with the intended darwin host listed under `hosts`:
   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#dotfiles-install -- $HOST
   ```

4. Apply:
   ```sh
   $ "$HOME"/.dotfiles/bin/sync.sh
   ```

5. Import the GPG Key using `gpg --import`. You may need to restart.
6. Reboot!

# Secrets

3. Enable git-filter:
```shell
$ ./bin/git-secret-filter.sh init
```

4. Test using:

# Docs & Acknowledgments

[Nix](https://nixos.org/) can be overwhelming with its steep learning curve. I found it easier reading documentation and ~some~ several dotfiles:
- [Nix Docs](https://nixos.org/guides/nix-pills/)
- [Home Manager Docs](https://nix-community.github.io/home-manager)
- [Nix Darwin Docs](https://daiderd.com/nix-darwin/manual/index.html)
- [Flakes Docs](https://nixos.wiki/wiki/Flakes)
- Several dotfiles (thank you!):
    - [`Misterio77`](https://github.com/Misterio77/nix-config) 
    - [`bbigras`](https://github.com/bbigras/nix-config)
    - [`malob`](https://github.com/malob/nixpkgs)
    - [`kclejeune`](https://github.com/kclejeune/system)
    - [`mitchellh`](https://github.com/mitchellh/nixos-config)
    - [`sei40kr`](https://github.com/sei40kr/dotfiles)
    - [`dustinlyons`](https://github.com/dustinlyons/nixos-config)
    - [`ambroisie`](https://git.belanyi.fr/ambroisie/nix-config/)

For more help on Nix(OS) seek out [the NixOS discourse](https://discourse.nixos.org).
