Hi! 👋

This is my personal [NixOS](https://nixos.org/) and [nix-darwin](https://github.com/LnL7/nix-darwin) that works _for me_. I hope this helps you!

<p float="center">
  <img src="hosts/laptop/screenshots/general.png" width="49%" />
  <img src="hosts/laptop/screenshots/music-menu.png" width="49%" />
</p>

## Hosts

| Host | Platform | Description |
|------|----------|-------------|
| [laptop](./hosts/laptop) | NixOS | Personal workstation |
| [work-macos](./hosts/work-macos) | Darwin | Work MacBook |
| [homelab](./hosts/homelab) | NixOS + Synology | Self-hosted infrastructure |

See [apps/](./apps) for installation scripts.

## Nix Stack

- [`disko`](https://github.com/nix-community/disko) for declarative disk partitioning
- [`stylix`](https://github.com/danth/stylix) for consistent theming in general
- [`sops-nix`](https://github.com/Mic92/sops-nix) for secrets
- [`nixos-anywhere`](https://github.com/nix-community/nixos-anywhere) to automate remote installations
- Not using [flake-utils](https://github.com/numtide/flake-utils) intentionally. I find it an _unnecessary_ abstraction
- Not using [impermanence](https://github.com/nix-community/impermanence). I tried and... it is _too much_

## Flake Outputs

For those familiar with [Nix Flakes](https://nixos.wiki/wiki/Flakes), take a look around at my [modules](./modules) and [packages](./packages):
```
$ nix run github:bphenriques/dotfiles#{package} -- {args}
```

You can see the list of packages by running:
```
$ nix flake show github:bphenriques/dotfiles
```

## Acknowledgements

Thanks to everyone sharing dotfiles, maintaining the [Arch Wiki](https://wiki.archlinux.org/) and [NixOS Wiki](https://wiki.nixos.org/), and helping in the [NixOS community](https://github.com/NixOS/nixpkgs?tab=readme-ov-file#community).
