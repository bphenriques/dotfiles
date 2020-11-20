[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to manage dotfiles.

<p align="center">
    <img src="./screenshots/macos.png" width="85%" />
</p>

----

# Setup

## NixOS

Soon™ :)

## MacOS

1. Register your's machine's SSH key on Github:
```sh
$ ssh-keygen -t ed25519 -C "bphenriques@outlook.com"
$ (cat "$HOME"/.ssh/id_ed25519.pub | pbcopy) && open https://github.com/settings/ssh/new
```

2. Clone the repository:
```sh
$ mkdir -p "$HOME"/workspace && cd "$_" && git clone git@github.com:bphenriques/dotfiles.git && cd dotfiles
```

3. Bootstrap the dependencies and sync the nix configuration:
```sh
$ make bootstrap sync-personal-mac
```

4. Import your personal GPG keys:
``` sh
$ gpg --import <key> 
```

5. Reboot!

# Updating

```sh
$ make update
```

This will update both `flake.lock` and Doom Emacs. Check if everything is stable before commiting.

# Troubleshooting

#### 1. Fail to find `nix`, `darwin`, `brew` nor any home-manager binary.

Make sure that you have the following in your `$ZDOTDIR/.zprofile` (here to ensure precedence):
```sh
# If nix can't be found
. /Users/$USER/.nix-profile/etc/profile.d/nix.sh

# If Home-Manager binaries can't be found
. /etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"

# If brew cant't be found
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
```

I suspect that this is related with some mess up when applying `nix-darwin`, maybe [this](https://github.com/LnL7/nix-darwin/pull/286) will fix.

#### 2. `zsh compinit: insecure directories, run compaudit for list.`

The mentioned directories are considered as they can be written by users that are not either the `root` or the current user ([source](http://zsh.sourceforge.net/Doc/Release/Completion-System.html##Use-of-compinit)). For this purpose, remove the rogue permissions:
```sh
compaudit | xargs chmod go-w
```

# Need some help?

[Nix](https://nixos.org/) can be overwhelming with its steep learning curve. I found it easier reading documentation and ~some~ several dotfiles:
- [Nix Docs](https://nixos.org/guides/nix-pills/)
- [Home Manager Docs](https://nix-community.github.io/home-manager)
- [Nix Darwin Docs](https://daiderd.com/nix-darwin/manual/index.html)
- [Flakes Docs](https://nixos.wiki/wiki/Flakes)
- [Flakes Introduction](https://www.tweag.io/blog/2020-05-25-flakes/).
- [`hlissner`](https://github.com/hlissner/dotfiles), [`malob`](https://github.com/malob/nixpkgs) and [`kclejeune`](https://github.com/kclejeune/system) dotfiles.

If you are new to dotfiles, I suggest looking for a more direct solution using a mixture of bare git repository and [`stow`](https://www.gnu.org/software/stow/) to symlink all the dotfiles. Start small and build up and let the tools work for you, not the way around :)

Feel free to contact me!
