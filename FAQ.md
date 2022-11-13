# FAQ

# Troubleshooting

#### 1. Fail to find `nix` nor any home-manager binary.

Add the following in your `$ZDOTDIR/.zprofile` (here to ensure precedence):
```sh
# Update PATH with Nix and Home-Manager packages
test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
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
