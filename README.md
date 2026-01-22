[![Laptop Host](https://img.shields.io/badge/Laptop-host-orange)](./hosts/laptop)
[![Work MacOS Host](https://img.shields.io/badge/Work_MacOS-host-orange)](./hosts/work-macos)
[![Packages](https://img.shields.io/badge/Packages-Flake-purple)](./packages)
[![Modules](https://img.shields.io/badge/Modules-Flake-purple)](./modules)
[![NixOS Install](https://img.shields.io/badge/NixOS_Install-docs-blue)](./apps/nixos-install/README.md)
[![Darwin Install](https://img.shields.io/badge/Darwin_Install-docs-blue)](./apps/darwin-install/README.md)

Hi! 👋 

This is my fleet configuration using [nix](https://nixos.org/) that works _for me_. I hope this helps you!

I try to have a minimalist and keyboard oriented setup. Take a look around and ask me anything!

<p float="center">
  <img src="hosts/laptop/screenshots/general.png" width="49%" />
  <img src="hosts/laptop/screenshots/music-menu.png" width="49%" />
  <em>Commit <a href=https://github.com/bphenriques/dotfiles/commit/db9275579208f7d1b4a3ef24aa01a2ab3ece9df4><code>db9275579208f7d1b4a3ef24aa01a2ab3ece9df4</code></a></em>
</p>

## Relevant Software

- **Compositor**: [`niri`](https://github.com/YaLTeR/niri)
- **Launcher**: [`fuzzel`](https://codeberg.org/dnkl/fuzzel)
- **Terminal**: [`foot`](https://codeberg.org/dnkl/foot)
- **Notifications**: [`dunst`](https://github.com/dunst-project/dunst)
- **Shell**: [`fish`](https://github.com/fish-shell/fish-shell)
- **File Manager**: [Nautilus](https://gitlab.gnome.org/GNOME/nautilus) but moving to [Yazi](https://github.com/sxyazi/yazi) _slowly_
- **Editor**: [IntelliJ IDEA](https://www.jetbrains.com/idea/) but moving to [Helix](https://github.com/helix-editor/helix) _slowly_
- **Music**: [Music Player Daemon](https://www.musicpd.org/) with a custom wrapper around [`mpc`](https://www.musicpd.org/clients/mpc/)
- **Internet Browser**: [Firefox](https://www.mozilla.org/firefox/new/) but exploring alternatives
- **Quick Menu**: [`wlr-which-key`](https://github.com/MaxVerevkin/wlr-which-key)

For those interested in the Nix aspects of the project:
- [`disko`](https://github.com/nix-community/disko) for declarative disk partitioning
- [`stylix`](https://github.com/danth/stylix) for consistent theming in general
- [`sops-nix`](https://github.com/Mic92/sops-nix) for secrets 
- [`nixos-anywhere`](https://github.com/nix-community/nixos-anywhere) to automate remote installations
- Not using [flake-utils](https://github.com/numtide/flake-utils) intentionally. I find it an _unnecessary_ abstraction
- Not using [impermanence](https://github.com/nix-community/impermanence). I tried and... it is _too much_

## Homelab

You will find the configuration of hosts part of my homelab:
- `storage`: a Synology DS923+ (how I started my homelab). While it can't use NixOS, I am consolidating the setup here.
- `compute`: a Beelink EQ14 as it is low powered and meets my needs.
- (future) `ai` once I can justify the (big) expense.

Having a unified project ensures everything is declaratively set together. The guidelines I am trying to follow:
1. **Security**: I am not an expert, but I do my best.
2. **3-2-1 Backups**: physical external drive and encrypted to ([backblaze](https://www.backblaze.com/)).
3. **Reproducible**: _For the most part_ everything should be set declaretively.

Stack:
- **Reverse proxy**: [`traefik`](https://github.com/traefik/traefik).
- **Authentication / Authorization**: [`pocket-id`](https://github.com/pocket-id/pocket-id) as OIDC provider for the apps that support it.
- **DNS registration**: [Cloudflare](./infrastructure/cloudflare.md). To be terraformed.
- **Remote access**: [Tailscale](./infrastructure/tailscale.md). Considering other options. To be terraformed.

## Flake Outputs

For those familiar with [Nix Flakes](https://nixos.wiki/wiki/Flakes), take a look around at my [modules](./modules) and [packages](./packages).

Listing: `nix flake show github:bphenriques/dotfiles`
Run packages: `nix run github:bphenriques/dotfiles#{package} -- {args}`

## Acknowledgements

I started this project to learn more about Linux: long, sometimes exhausting but enlightening. 

I want to thank everyone that sharing their own dotfiles, those writing great documentation ([Arch Linux Wiki](https://wiki.archlinux.org/title/Main_page) and [NixOS Wiki](https://wiki.nixos.org/wiki/NixOS_Wiki)), and those [available to help](https://github.com/NixOS/nixpkgs?tab=readme-ov-file#community).
