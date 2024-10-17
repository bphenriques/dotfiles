[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 

This repository contains the definition of my [nix-managed](https://nixos.org/) machines.

> [!IMPORTANT]
> **Disclaimer:** This is my personal configuration that works _for me_. I hope this helps you!
> 
> If you are new to dotfiles in general, I suggest a bare git solution to start with.

----

What you will find here:
- Using [nixos-anywhere](https://github.com/nix-community/nixos-anywhere) to automate the installation remotely.
- Using disko.
- [sops-nix](https://github.com/Mic92/sops-nix) for critical secrets that I do not want in the nix store.
- Combination of `git-filter` and [sops](https://github.com/getsops/sops) for non-critical sensitive information required in Nix evaluation time that I do not mind being in plain-text in the nix store.

|   Hostname  |               CPU              |  RAM  |         Primary GPU         |      Secondary GPU      | Role | OS  |
| :---------: | :----------------------------: | :---: | :-------------------------: | :---------------------: | :--: | :-: |
| `laptop`   | AMD Ryzen™ 7 7840HS           | 32GB  | AMD Ryzen™ 7 7840HS | NVIDIA® GeForce RTX™ 4060 8GB | Personal | ❄️  |
| `work-macos`     | Apple M2 8-core CPU            | 16GB  | Apple M2 10-core GPU        |                         | Work | 🍏  |

# Installing NixOS

## From another machine

Requirements:
- **Source machine**: the machine used to create a bootable USB and remotely trigger the NixOS installation.
- **Target machine**: the new machine where we will install NixOS.

1. On the source machine, create a bootable USB [installer](https://nixos.org/download/):

   ```
   $ sudo fdisk -l
   $ sudo dd bs=4M if=<ISO> of=<PEN_DRIVE> status=progress oflag=sync
   ```

2. On the target machine:
   1. Set `nixos`'s password using `passwd`. 
   2. Setup network connection (relevant if on wifi)
   3. Note down the hardware/network information:

   ```
   $ nixos-generate-config --no-filesystems --root /mnt --show-hardware-config
   $ lsblk -p
   $ ip route get 1.2.3.4 | awk '{print $7}'
   ```

3. On the source machine:
   1. Clone this repository.
   2. Duplicate one of the NixOS hosts configuration folder and add an entry in `flake.nix`.
   3. Set the `hardware-configuration.nix`.
   4. Review the disk layout under `disk-config.nix` (see [disko](https://github.com/nix-community/disko)).
   5. (opt): Set secrets:
      1. Add the shared public key `age160xsly2d84lz89vzhkussw3pth2fhkstx03cd7uc5df6zdpsfvnsktf7hs` in `.sops.yaml`.
      2. Add if missing the corresponding private key under `$HOME/.config/sops/age/keys.txt`
      3. Initialize a secret file using `sops host/<HOST>/secrets/sops.yaml`.
   6. Fine-tune the configuration.
   7. Commit the changes (optionally push).


```shell
bw login
bw unlock
nix run --extra-experimental-features 'nix-command flakes' ".#nixos-install" -- remote-install #.laptop nixos@192.168.68.62 \
  --sops-age-private_key "$(bw get item sops-age-key-laptop | jq --raw-output '.fields[] | select(.name=="private-key") | .value')" \
  --sops-age-private_key /persist/config/bphenriques/home/bphenriques/.config/sops/age/keys.txt
```

5. In the source machine run the following (replace `<HOST>` and `<IP>`). The script automatically generates a SSH key and retrieves credentials from my secret vault:

    When using impermanence:
    ```
    $ ./bin/deploy-machine-with-secrets.sh .#<HOST> root@<IP> /persist/config/bphenriques/home/bphenriques
    ```

    When not using impermanence:
    ```
    $ ./bin/deploy-machine-with-secrets.sh .#<HOST> root@<IP> /home/bphenriques
    ```

6. Once the initial installation succeeds, the `.dotfiles` repository should be available.

TODO: Run the `./bin/git-secret-filter.sh init`
TODO: Add this command by default on all scripts: `--experimental-features "nix-command flakes"`

# Installing on Darwin

1. Ensure [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html) is installed.

2. Boostrap:

   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' ".#darwin-install"
   ```
   
3. Setup this dotfiles repository. Replace `{host}` with the intended darwin host listed under `hosts`:

   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' ".#dotfiles-install" -- --host {host}
   ```

4. Apply:
   ```sh
   $ "$HOME"/.dotfiles/bin/sync.sh
   ```

4. Import the GPG Key using `gpg --import`. You may need to restart.
   
5. Reboot!

To setup `git-filter`:
```shell
$ ./bin/git-secret-filter.sh init
```

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
