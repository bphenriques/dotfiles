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

- | Hostname     | CPU                     | RAM  | Primary GPU              | Secondary GPU                 | OS |
|--------------|-------------------------|------|--------------------------|-------------------------------|----|
| `laptop`     | AMD Ryzen™ 7 7840HS     | 32GB | AMD Radeon™ 780M | NVIDIA® GeForce RTX™ 4060 8GB | ❄️  |
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

2. On the target machine:
   1. Set `nixos`'s password using `passwd`.
   2. Set up network connection note down its address:`ip route get 1.2.3.4 | awk '{print $7}'`

3. On the source machine:
   1. Clone this repository.
   2. Duplicate one of the NixOS hosts configuration folder and add an entry in `flake.nix`.
   3. Export `hardware-configuration.nix` as follows:
   ```
   ssh <host> -- nixos-generate-config --no-filesystems --root /mnt --show-hardware-config
   ```
   
   4. Update `disk-config.nix` ([disko](https://github.com/nix-community/disko)) considering the disks available:
   ```
   ssh <host> -- lsblk -p
   ```

4. In the source machine run the following (replace `<HOST>` and `<IP>`). The script automatically generates a SSH key and retrieves credentials from my secret vault:

    ```
    bw login
    bw unlock
    nix run --extra-experimental-features 'nix-command flakes' ".#nixos-install" -- remote-install #.laptop nixos@192.168.68.62 \
       --sops-age-private_key "$(bw get item sops-age-key-laptop | jq --raw-output '.fields[] | select(.name=="private-key") | .value')" \
       --sops-age-private_key /persist/config/bphenriques/home/bphenriques/.config/sops/age/keys.txt
    
    $ ./bin/deploy-machine-with-secrets.sh .#<HOST> root@<IP> /persist/config/bphenriques/home/bphenriques
    ```

6. Once the initial installation succeeds, run the dotfiles installer as follows:
    ```
    $ nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install" -- work-macos
    $ nix run --extra-experimental-features 'nix-command flakes' ".#dotfiles-install" -- \
        work-macos --dotfiles-location /tmp/test4/.dotfiles --ssh-directory /tmp/test4/.ssh --ssh-key-comment "hello" \
        --age-keys-file ...
    ```

# Installing on Darwin

1. Ensure [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html) is installed.
2. Boostrap:
   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#darwin-install"
   ```
   
3. Setup this dotfiles repository. Replace `{host}` with the intended darwin host listed under `hosts`:
   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles-install"
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


Disclaimer: I do not claim ownership of the wallpapers appearing in this repository. If you find images in this repository owned by you and are of limited use, please let me know and I will remove them.
