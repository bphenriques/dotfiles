[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to set working environments.

<p align="center">
    <img src="./screenshots/macos.png" width="85%" />
</p>

----

# Pre-Requirements

1. Ensure `nix` is installed and sourced: https://nixos.org/manual/nix/stable/installation/installing-binary.html

   ```aidl
   test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
   ```

# Setup

| Host | Operating System |
|-|-|
| [`personal-macos`](hosts/personal-macos.nix) | macOS |
| [`work-macos`](hosts/work-macos.nix) | macOS |
| [`wsl`](hosts/wsl.nix) | Ubuntu (WSL) |

1. Bootstrap:
```sh
$ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/bphenriques/dotfiles/master/scripts/bootstrap.sh)"
```

2. Sync flake:
```sh
$ cd "$HOME"/.dotfiles
$ make sync
```

3. Import the GPG keys:
```sh
$ pbpaste  | gpg --import
```

4. Reboot!

# Updating

```sh
$ make update
```

This will update both `flake.lock` and Doom Emacs. Check if everything is stable before committing.

It is also possible to see the list of updated packages between Nix generations:
```sh
$ nix profile diff-closures --profile /nix/var/nix/profiles/system
```

# Acknowledgments

[Nix](https://nixos.org/) can be overwhelming with its steep learning curve. I found it easier reading documentation and ~some~ several dotfiles:
- [Nix Docs](https://nixos.org/guides/nix-pills/)
- [Home Manager Docs](https://nix-community.github.io/home-manager)
- [Nix Darwin Docs](https://daiderd.com/nix-darwin/manual/index.html)
- [Flakes Docs](https://nixos.wiki/wiki/Flakes)
- [Flakes Introduction](https://www.tweag.io/blog/2020-05-25-flakes/).
- Several dotfiles:
    - [`hlissner`](https://github.com/hlissner/dotfiles)
    - [`malob`](https://github.com/malob/nixpkgs)
    - [`kclejeune`](https://github.com/kclejeune/system)
    - [`mitchellh`](https://github.com/mitchellh/nixos-config)
    - [`mjlbach`](https://github.com/mjlbach/nix-dotfiles)
    - [`sei40kr`](https://github.com/sei40kr/dotfiles)
    - [`samuelludwig`](https://github.com/samuelludwig/nixrc)
    - [`jacobfoard`](https://github.com/jacobfoard/dotfiles)

If you are new to dotfiles, I suggest looking for a more direct solution using a mixture of bare git repository and [`stow`](https://www.gnu.org/software/stow/) to symlink all the dotfiles. Start small and build up and make the tools work for you :)

Feel free to contact me if you need help!
