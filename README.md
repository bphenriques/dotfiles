[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to set working environments.

----

# Quick start

> [!IMPORTANT]
> **Disclaimer:** This is a personal configuration. I am no expert, however 
> I hope that this helps you! For more help on Nix(OS) seek out [the NixOS discourse](https://discourse.nixos.org).
> If you are new to dotfiles in general, use a bare git solution to start with and built it from there. Make the tools work for you rather than the other way around.

# NixOS

Using [nixos-anywhere](https://github.com/nix-community/nixos-anywhere) to automate the installation. Requires two machines:
- **Target**: the new machine that we will be installing NixOS.
- **Source**: the current machine where we will create a bootable USB and remotely install the operating system.

1. On the source machine, create a bootable USB [installer](https://nixos.org/download/):

   ```
   $ sudo fdisk -l
   $ sudo dd bs=4M if=<ISO> of=<MOUNTED_USB> status=progress oflag=sync
   ```

2. On the target machine, setup networking and then note down the hardware/network information:

   ```
   $ nixos-generate-config --no-filesystems --root /mnt --show-hardware-config
   $ lsblk -p
   $ ip route get 1.2.3.4 | awk '{print $7}'
   ```
   
3. On the target machine, set the SSH password of the `nixos` user (`passwd`). You can set 

4. On the source machine:
   1. Clone this repository.
   2. Duplicate one of the NixOS hosts configuration folder and add an entry in `flake.nix`.
   3. Set the `hardware-configuration.nix`.
   4. Review the disk layout under `disk-config.nix` (see [disko](https://github.com/nix-community/disko)).
   5. Ensure `users.users.root.openssh.authorizedKeys.keys` contains the public SSH key of the source machine.
   6. Push the changes

4. Finally, run the following in the source machine (replace `<HOST>` and `<IP>`) in the repository directory:

   ```
   $ nix run github:nix-community/nixos-anywhere -- --flake ".#<HOST>" root@<IP>
   ```

PS: It is possible to [make my own NixOS image installer](https://nixos.org/manual/nixos/stable/index.html#sec-building-image), but I find the `nix-community` one sufficient.


$ nix run github:nix-community/nixos-anywhere -- --flake #laptop nixos@192.168.68.55

# Non NixOS machines

1. Ensure [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html) is installed.

2. Bootstrap:

   ```sh
   $ nix-shell --packages git --command "$(curl -fsSL https://raw.githubusercontent.com/bphenriques/dotfiles/master/bin/bootstrap.sh)"
   ```

3. Apply:
   ```sh
   $ "$HOME"/.dotfiles/bin/sync.sh
   ```

4. Import the GPG Key using `gpg --import`. You may need to restart.
   
5. Reboot!

# Secrets

[sops-nix](https://github.com/Mic92/sops-nix) for critical secrets and `age`+`git-filter` (`smudge` `clean`) for non-critical sensitive information required in Nix evaluation time.

To setup `git-filter`:
```shell
$ ./bin/git-secret-filter.sh init
```

As detailed in `.gitattributes`, only `*.age.nix` are affected. My public keys under `.sops.yaml` and the corresponding private keys under `"$XDG_CONFIG_HOME/sops/age/keys.txt"`.

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

Disclaimer: I do not claim ownership of the wallpapers appearing in this repository. If you find images in this repository owned by you and are of limited use, please let me know and I will remove them.
