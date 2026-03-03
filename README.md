# NixOS & Darwin Dotfiles

[![Modules](https://img.shields.io/badge/Modules-Flake-purple)](./modules)
[![Packages](https://img.shields.io/badge/Packages-Flake-purple)](./packages)
[![NixOS Install](https://img.shields.io/badge/NixOS_Install-docs-blue)](./apps/nixos-install/README.md)
[![Darwin Install](https://img.shields.io/badge/Darwin_Install-docs-blue)](./apps/darwin-install/README.md)

Hi! 👋 This is my personal [NixOS](https://nixos.org/) and [nix-darwin](https://github.com/LnL7/nix-darwin) flake that works _for me_. I hope this helps you!

<p float="center">
  <img src="hosts/laptop/screenshots/general.png" width="49%" />
  <img src="hosts/laptop/screenshots/music-menu.png" width="49%" />
</p>

## Hosts

| Host | Platform | Description |
|------|----------|-------------|
| [homelab](./hosts/homelab) | NixOS + Synology | Self-hosted infrastructure |
| [laptop](./hosts/laptop) | NixOS | Personal workstation |
| [work-macos](./hosts/work-macos) | Darwin | Work MacBook |

## Nix Stack

Layout:
- [`hosts/`](./hosts) — per-host configurations (hardware, services, users)
- [`modules/`](./modules) — reusable modules that *define* options (e.g., `custom.homelab.*`)
- [`profiles/`](./profiles) — shared opinionated configuration that *sets* standard options (imported by hosts)
- [`packages/`](./packages) — custom packages and scripts
- [`lib/`](./lib) — custom helpers and builders
- [`apps/`](./apps) — runnable scripts (installation, post-install)

Key dependencies:
- [`disko`](https://github.com/nix-community/disko) for declarative disk partitioning
- [`stylix`](https://github.com/danth/stylix) for consistent theming
- [`sops-nix`](https://github.com/Mic92/sops-nix) for secrets
- [`nixos-anywhere`](https://github.com/nix-community/nixos-anywhere) for remote installations

Not using [flake-utils](https://github.com/numtide/flake-utils) or [impermanence](https://github.com/nix-community/impermanence) intentionally.

## Flake Outputs

```
$ nix flake show github:bphenriques/dotfiles
$ nix run github:bphenriques/dotfiles#<package>
```

## Acknowledgements

Thanks to everyone sharing dotfiles, maintaining the [Arch Wiki](https://wiki.archlinux.org/) and [NixOS Wiki](https://wiki.nixos.org/), and helping in the [NixOS community](https://github.com/NixOS/nixpkgs?tab=readme-ov-file#community).
