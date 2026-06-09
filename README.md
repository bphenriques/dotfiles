# bphenriques's fleet

[![selfhost-nix](https://img.shields.io/badge/selfhost--nix-Framework-purple)](https://github.com/bphenriques/selfhost-nix)
[![Modules](https://img.shields.io/badge/Modules-Flake-purple)](./modules)
[![Packages](https://img.shields.io/badge/Packages-Flake-purple)](./packages)
[![NixOS Install](https://img.shields.io/badge/NixOS_Install-docs-blue)](./apps/nixos-install/README.md)

Hi! 👋 This is how I am managing my personal machines using [NixOS](https://nixos.org/). I hope this helps you!

<p float="center">
  <img src="hosts/compute/screenshots/homepage.png" width="49%" />
  <img src="hosts/compute/screenshots/grafana.png" width="46%" />
</p>

<p float="center">
  <img src="hosts/laptop/screenshots/general.png" width="49%" />
  <img src="hosts/laptop/screenshots/music-menu.png" width="49%" />
</p>

## Hosts

| Host                       | Platform     | Description                                                                                                                    |
|----------------------------|--------------|--------------------------------------------------------------------------------------------------------------------------------|
| [compute](./hosts/compute) | NixOS        | Homelab using my [`selfhost-nix`](https://github.com/bphenriques/selfhost-nix) flake (Traefik + OIDC SSO + Automated Secrets). |
| [laptop](./hosts/laptop)   | NixOS        | Personal workstation                                                                                                           |
| [inky](./hosts/inky)       | Raspberry Pi | Raspberry Pi Zero 2W connected to Inky Impression and speakers                                                                 |

## Nix Stack

Layout:

- [`hosts/`](./hosts): per-host configurations (hardware, services, users)
- [`modules/`](./modules): personal NixOS/home-manager modules (the `selfhost.*` framework lives in [selfhost-nix](https://github.com/bphenriques/selfhost-nix))
- [`profiles/`](./profiles): shared opinionated configuration that *sets* standard options (imported by hosts)
- [`packages/`](./packages): custom packages and scripts
- [`lib/`](./lib): custom helpers and builders
- [`apps/`](./apps): runnable scripts (installation, post-install)

Key dependencies:

- [`disko`](https://github.com/nix-community/disko) for declarative disk partitioning
- [`stylix`](https://github.com/danth/stylix) for consistent theming
- [`sops-nix`](https://github.com/Mic92/sops-nix) for secrets
- [`nixos-anywhere`](https://github.com/nix-community/nixos-anywhere) for remote installations
- [`selfhost-nix`](https://github.com/bphenriques/selfhost-nix) that abstracts most concerns about basic self-hosting
- `dotfiles-private` that is a private repository containing private information

Not using [flake-utils](https://github.com/numtide/flake-utils) or [impermanence](https://github.com/nix-community/impermanence) intentionally.

<details>
<summary>Structure of <code>dotfiles-private</code></summary>

```
.
├── flake.lock
├── flake.nix
├── hosts
│   ├── compute
│   │   ├── default.nix
│   │   ├── secrets.yaml    <- Encrypted
│   │   ├── settings.nix
│   │   └── users
│   │       ├── bphenriques.nix
│   │       └── johndoe.nix
│   └── laptop
│       ├── default.nix
│       └── secrets.yaml    <- Encrypted
├── packages
│   └── wallpapers
│       ├── default.nix
│       └── src
│           ├── beach-night-sky.jpg
│           ├── ...
│           └── watch-tower.png
├── README.md
└── shell.nix               <- With sops package inside
```

</details>

## Workflow

A [`dot`](./packages/dotfiles/dotfiles) CLI wraps the native rebuild tools:

```bash
dot . s              # build, preview changes, and apply to the current host
dot . b              # build and preview changes without applying
dot . u              # update flake inputs
dot . c              # show changelog between last two local profiles
dot compute s        # deploy to the compute host remotely
dot compute c        # show changelog for the compute host
```

## AI Disclaimer

AI was used from January 2026 onwards, starting with the [`compute`](./hosts/compute) host to learn and iterate faster. I drive the architecture, review and own every line.

## Acknowledgements

Thanks to everyone sharing dotfiles, maintaining the [Arch Wiki](https://wiki.archlinux.org/) and [NixOS Wiki](https://wiki.nixos.org/), and helping in the [NixOS community](https://github.com/NixOS/nixpkgs?tab=readme-ov-file#community).
