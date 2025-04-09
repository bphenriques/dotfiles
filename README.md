[![Laptop Host](https://img.shields.io/badge/Laptop-host-orange)](./hosts/laptop)
[![Work MacOS Host](https://img.shields.io/badge/Work_MacOS-host-orange)](./hosts/work-macos)
[![Packages](https://img.shields.io/badge/Packages-Flake-purple)](./packages)
[![Modules](https://img.shields.io/badge/Modules-Flake-purple)](./modules)
[![NixOS Install](https://img.shields.io/badge/NixOS_Install-docs-blue)](./apps/nixos-install/README.md)
[![Darwin Install](https://img.shields.io/badge/Darwin_Install-docs-blue)](./apps/darwin-install/README.md)

Hi! 👋 

This is my personal configuration using [nix](https://nixos.org/) that works _for me_. I hope this helps you!

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
- [`nur`](https://github.com/nix-community/NUR) to get access to some packages (e.g., Firefox addons)
- Not using [flake-utils](https://github.com/numtide/flake-utils) intentionally. I find it an _unnecessary_ abstraction
- Not using [impermanence](https://github.com/nix-community/impermanence). I tried and... it is _too much_

## Flake Outputs

For those familiar with [Nix Flakes](https://nixos.wiki/wiki/Flakes), take a look around at my [modules](./modules) and [packages](./packages):
```
$ nix run github:bphenriques/dotfiles#{package}
```

You can see the list of packages by running:
```
$ nix flake show github:bphenriques/dotfiles
```

## Acknowledgements and license

My journey to move to Linux has been long, sometimes exhausting but enlightening. This would not be possible without the work of other developers in exposing their own dotfiles, writing great documentation ([Arch Linux Wiki](https://wiki.archlinux.org/title/Main_page) and [NixOS Wiki](https://wiki.nixos.org/wiki/NixOS_Wiki)), and [being available to help](https://github.com/NixOS/nixpkgs?tab=readme-ov-file#community). 

Thank you! It is my turn to share my personal under the MIT license, which is only relevant to what I have built: [packages](./packages), some [modules](./modules) (that I should contribute upstream), and some [helpful functions](./lib).
