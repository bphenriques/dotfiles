[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to set working environments.

----

# Pre-Requirements

Install [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html) and source it:
   ```shell
   $ . "$HOME"/.nix-profile/etc/profile.d/nix.sh
   ```

# Setup

In the [`host`](https://github.com/bphenriques/dotfiles/tree/master/host) directory you'll find the available hosts.

1. Bootstrap:
   ```sh
   $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/bphenriques/dotfiles/master/bin/bootstrap.sh)"
   ```

2. Sync flake:
   ```sh
   $ "$HOME"/.dotfiles/bin/sync.sh
   ```

3. Import the GPG Key:
   
   From clipboard:
   ```sh
   $ pbpaste | gpg --import
   ```

   From file:
   ```sh
   $ gpg --import private.key
   ```
   
4. Reboot!

# Updating

The following will update `flake.lock` and Doom Emacs.
```sh
$ dotfiles update
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
    - [`sei40kr`](https://github.com/sei40kr/dotfiles)
    - [`samuelludwig`](https://github.com/samuelludwig/nixrc)
    - [`jacobfoard`](https://github.com/jacobfoard/dotfiles)
    - [`dustinlyons`](https://github.com/dustinlyons/nixos-config)
- Ad-hoc documentation for reference that I find useful:
  - https://nixpkgs-manual-sphinx-markedown-example.netlify.app/development/option-types.xml.html
  - https://teu5us.github.io/nix-lib.html#builtins.replacestrings
  - https://nixos.org/manual/nix/stable/language/operators.html
  - https://nixos.org/manual/nix/stable/language/builtins.html
  - https://github.com/NixOS/nixpkgs/blob/master/lib/strings.nix
  - https://github.com/NixOS/nixpkgs/blob/master/lib/types.nix

If you are new to dotfiles, I suggest looking for a more direct solution using a bare git repository. Start small and build up and make the tools work for you.

Feel free to contact me if you need help!
