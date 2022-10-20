# FAQ

#### 1. Uninstall Nix

Non-comprehensive guide. 

1. Remove [nix-darwin](https://github.com/LnL7/nix-darwin#uninstalling).
2. `rm -rf $HOME/{.nix-channels,.nix-defexpr,.nix-profile,.config/nixpkgs}`
3. Reboot.
4. On Disk Utility, unmount Nix Storage
5. Remove Nix Storage
6. Check that `/etc/synthetic.conf` does not contain Nix. If so, remove it. Reboot.
7. `sudo rm -rf /nix`

If on multi-user installation:
```
for num in {1..32}; do sudo dscl . -delete /Users/nixbld$num; done
sudo dscl . -delete /Groups/nixbld
```

Follows main sources:
- [https://github.com/NixOS/nix/issues/1402]()
- [https://gist.github.com/expelledboy/c00aebb004b178cf78b2c9b344526ff6]()

# Troubleshooting

#### 1. Fail to find `nix` nor any home-manager binary.

Add the following in your `$ZDOTDIR/.zprofile` (here to ensure precedence):
```sh
# Source Nix and Home-Manager
test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
test -f /etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh && . /etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh
```

#### 2. `zsh compinit: insecure directories, run compaudit for list.`

The mentioned directories are considered as they can be written by users that are not either the `root` or the current user ([source](http://zsh.sourceforge.net/Doc/Release/Completion-System.html##Use-of-compinit)). For this purpose, remove the rogue permissions:
```sh
compaudit | xargs chmod go-w
```

### 3. Nix Darwin warning regarding `/etc/nix/nix.conf`

This file already exists after first install. Back it up and re-apply nix-darwin:
```shell
sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.bkup
```

See https://github.com/LnL7/nix-darwin/issues/458

### 5. Can't run `p10k configure` as the `/nix/store/` is readonly

Manually change the p10k location and run the command ([source](https://github.com/romkatv/powerlevel10k/issues/967)):
```shell
$ POWERLEVEL9K_CONFIG_FILE=hello.p10k p10k configure
```
