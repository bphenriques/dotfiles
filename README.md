[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to set working environments.

----

# FIXME: quick idea of hte README: https://github.com/Prometheus7435/nix-config/blob/main/README.org

# Quick start

> [!IMPORTANT]
> **Disclaimer:** This is a personal configuration. I am no expert, however 
> I hope that this helps you! For more help on Nix(OS) seek out [the NixOS discourse](https://discourse.nixos.org).
> If you are new to dotfiles in general, use a bare git solution to start with and built it from there. Make the tools work for you rather than the other way around.

What you will find here:
- Using [nixos-anywhere](https://github.com/nix-community/nixos-anywhere) to automate the installation.
- Two types of secret management:
  - [sops-nix](https://github.com/Mic92/sops-nix) for critical secrets that I do not want in the nix store.
  - Combination of `age` and `git-filter` for non-critical sensitive information required in Nix evaluation time that I do not mind being in plain-text in the nix store.

The public keys are under `.sops.yaml` and the private keys under `"$XDG_CONFIG_HOME/sops/age/keys.txt"`.


# NixOS

Using [nixos-anywhere](https://github.com/nix-community/nixos-anywhere) to automate the installation. Requires two machines:
- **Target**: the new machine that we will be installing NixOS.
- **Source**: the current machine where we will create a bootable USB and remotely install the operating system.

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

# Darwin

1. Ensure [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html) is installed.

2. Boostrap:

   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' ".#darwin-install"
   ```
   
3. Install the dotfiles repository:

   ```sh
   $ nix run --extra-experimental-features 'nix-command flakes' ".#dotfiles-install" -- --host work-macos
   ```

4. Apply:
   ```sh
   $ "$HOME"/.dotfiles/bin/sync.sh
   ```

4. Import the GPG Key using `gpg --import`. You may need to restart.
   
5. Reboot!

# Secrets

- [sops-nix](https://github.com/Mic92/sops-nix) for critical secrets that I do not want in the nix-store
- `age`+`git-filter` (`smudge` `clean`) for non-critical sensitive information required in Nix evaluation time that I do not mind being in plain-text.
  As detailed in `.gitattributes`, only `*.age.nix` are affected.

The public keys are under `.sops.yaml` and the private keys under `"$XDG_CONFIG_HOME/sops/age/keys.txt"`.

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

Disclaimer: I do not claim ownership of the wallpapers appearing in this repository. If you find images in this repository owned by you and are of limited use, please let me know and I will remove them.
