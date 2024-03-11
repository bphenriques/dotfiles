[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to set working environments.

----

# Quick start

> [!IMPORTANT]
> **Disclaimer:** This is a personal configuration. I am no expert, however 
> I hope that this helps you! For more help on Nix(OS) seek out [the NixOS discourse](https://discourse.nixos.org).
> If you are new to dotfiles in general, use a bare git solution to start with and built it from there. Make the tools work for you rather than the other way around.

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

In addition to [sops-nix](https://github.com/Mic92/sops-nix) for critical secrets and combination of `age` and `git-filter` (`smudge` `clean`) to hide sensitive information required in Nix evaluation time.

To setup `git-filter` (`smudge` `clean`) initialize it:
```shell
$ ./bin/git-secret-filter.sh init
```

And, as detailed in `.gitattributes`, only `*.age.nix` are affected. My public keys under `.sops.yaml` and the corresponding private keys under `"$XDG_CONFIG_HOME/sops/age/keys.txt"`.

# Docs & Acknowledgments

[Nix](https://nixos.org/) can be overwhelming with its steep learning curve. I found it easier reading documentation and ~some~ several dotfiles:
- [Nix Docs](https://nixos.org/guides/nix-pills/)
- [Home Manager Docs](https://nix-community.github.io/home-manager)
- [Nix Darwin Docs](https://daiderd.com/nix-darwin/manual/index.html)
- [Flakes Docs](https://nixos.wiki/wiki/Flakes)
- [Flakes Introduction](https://www.tweag.io/blog/2020-05-25-flakes/).
- Several dotfiles (thank you!):
    - [`Misterio77`](https://github.com/Misterio77/nix-config) 
    - [`bbigras`](https://github.com/bbigras/nix-config)
    - [`malob`](https://github.com/malob/nixpkgs)
    - [`kclejeune`](https://github.com/kclejeune/system)
    - [`mitchellh`](https://github.com/mitchellh/nixos-config)
    - [`sei40kr`](https://github.com/sei40kr/dotfiles)
    - [`dustinlyons`](https://github.com/dustinlyons/nixos-config)
    - [`ambroisie`](https://git.belanyi.fr/ambroisie/nix-config/)
- Ad-hoc documentation for reference that I find useful:
  - https://nixpkgs-manual-sphinx-markedown-example.netlify.app/development/option-types.xml.html
  - https://teu5us.github.io/nix-lib.html#builtins.replacestrings
  - https://nixos.org/manual/nix/stable/language/operators.html
  - https://nixos.org/manual/nix/stable/language/builtins.html
  - https://github.com/NixOS/nixpkgs/blob/master/lib/strings.nix
  - https://github.com/NixOS/nixpkgs/blob/master/lib/types.nix
