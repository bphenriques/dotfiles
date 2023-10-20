[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to set working environments.

----

# Quick start

> [!IMPORTANT]
> **Disclaimer:** This is a personal configuration. I am no expert, however 
> I hope that this configuration helps you out setting up your own! For more help on Nix(OS) seek out [the NixOS discourse](https://discourse.nixos.org).
> If you are new to dotfiles in general, use a bare git solution to start with and built it from there. Make the tools work for you rather than the other way around.

1. If not available, install [`nix`](https://nixos.org/manual/nix/stable/installation/installing-binary.html).

2. Bootstrap:

   ```sh
   $ nix-shell --packages git --command "$(curl -fsSL https://raw.githubusercontent.com/bphenriques/dotfiles/master/bin/bootstrap.sh)"
   ```

3. Apply:
   ```sh
   $ "$HOME"/.dotfiles/bin/sync.sh
   ```

4. Import the GPG Key using `gpg --import`. If pinentry fails, you may need to restart.
   
5. Reboot!

# Updating

The following will update `flake.lock`:
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
- Several dotfiles (thank you!):
    - [`hlissner`](https://github.com/hlissner/dotfiles)
    - [`malob`](https://github.com/malob/nixpkgs)
    - [`kclejeune`](https://github.com/kclejeune/system)
    - [`mitchellh`](https://github.com/mitchellh/nixos-config)
    - [`sei40kr`](https://github.com/sei40kr/dotfiles)
    - [`samuelludwig`](https://github.com/samuelludwig/nixrc)
    - [`jacobfoard`](https://github.com/jacobfoard/dotfiles)
    - [`dustinlyons`](https://github.com/dustinlyons/nixos-config)
    - [`ambroisie`](https://git.belanyi.fr/ambroisie/nix-config/)
- Ad-hoc documentation for reference that I find useful:
  - https://nixpkgs-manual-sphinx-markedown-example.netlify.app/development/option-types.xml.html
  - https://teu5us.github.io/nix-lib.html#builtins.replacestrings
  - https://nixos.org/manual/nix/stable/language/operators.html
  - https://nixos.org/manual/nix/stable/language/builtins.html
  - https://github.com/NixOS/nixpkgs/blob/master/lib/strings.nix
  - https://github.com/NixOS/nixpkgs/blob/master/lib/types.nix
