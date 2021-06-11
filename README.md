[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Hi! 👋 Welcome to my repository containing my [Nix](https://nixos.org/) configurations to manage my machines. A declarative and _more_ reproducible way to setup working environments.

<p align="center">
    <img src="./screenshots/macos.png" width="85%" />
</p>

----

# Setup

| Host | Operating System |
|-|-|
| [`personal-macos`](hosts/personal-macos.nix) | macOS |
| [`work-macos`](hosts/work-macos.nix) | macOS |

If you are looking for the regular dotfiles, check [home/config](home/config).

## MacOS

If using M1:
1. Enable Rosetta:
```sh
$ /usr/sbin/softwareupdate --install-rosetta --agree-to-license
``` 
2. Duplicate `Terminal` App and rename the duplicate to `Terminal (Rosetta)`.
3. Under `Get Info`, set the application to open using Rosetta.
4. Open Terminal Rosetta.

Now the setup itself:

1. Register your's machine's SSH key on Github, replace `<email-address>` with your email address:
```sh
$ ssh-keygen -t ed25519 -C "<email-address>"
$ (cat "$HOME"/.ssh/id_ed25519.pub | pbcopy) && open https://github.com/settings/ssh/new
```

2. Clone the repository:
```sh
$ mkdir -p "$HOME"/workspace && cd "$_" && git clone git@github.com:bphenriques/dotfiles.git && cd dotfiles
```

3. Bootstrap the dependencies and sync the nix configuration:
```sh
$ make bootstrap sync-<host>
```

4. Import your public GPG key:
```sh
$ gpg --import <public-key-location>
```

5. Import your private GPG key:
``` sh
$ base64 -d <private-key-location> | gpg --import
```

**Warning**: Do not forget to delete the GPG keys.

1. Reboot!

# Updating

```sh
$ make update sync-<host>
```

This will update both `flake.lock` and Doom Emacs. Check if everything is stable before commiting.

# Troubleshooting

#### 1. Fail to find `nix` nor any home-manager binary.

Make sure that you have the following in your `$ZDOTDIR/.zprofile` (here to ensure precedence):
```sh
# If nix can't be found
. /Users/$USER/.nix-profile/etc/profile.d/nix.sh

# If Home-Manager binaries can't be found
. /etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
```

I suspect this is related with [this](https://github.com/LnL7/nix-darwin/pull/286).

#### 2. Fail to find `brew`.

Make sure that `/etc/zprofile` is calling `/usr/libexec/path_helper` as follows:
```sh
if [ -x /usr/libexec/path_helper ]; then
        eval `/usr/libexec/path_helper -s`
fi
```

You might need to create copy `/etc/zprofile.orig` to `/etc/zprofile`:
```sh
$ sudo cp /etc/zprofile.orig /etc/zprofile
```

Or if it does not work, add the following:
```sh
CPU=$(uname -p)
if [[ "$CPU" == "arm" ]]; then
    eval $(/opt/homebrew/bin/brew shellenv)
else
    eval $(/usr/local/bin/brew shellenv)
fi
```

#### 3. `zsh compinit: insecure directories, run compaudit for list.`

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
- Among many, [`hlissner`](https://github.com/hlissner/dotfiles), [`malob`](https://github.com/malob/nixpkgs) and [`kclejeune`](https://github.com/kclejeune/system) dotfiles.

If you are new to dotfiles, I suggest looking for a more direct solution using a mixture of bare git repository and [`stow`](https://www.gnu.org/software/stow/) to symlink all the dotfiles. Start small and build up and make the tools work for you :)

Feel free to contact me if you need help!
