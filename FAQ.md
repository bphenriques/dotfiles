# FAQ

#### Fail to find `nix` nor any home-manager binary.

Add the following in your default shell you have the following:
```sh
# Update PATH with Nix and Home-Manager packages
test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
```

Once done, you run the existing commands.

#### `zsh compinit: insecure directories, run compaudit for list.`

The mentioned directories are mentioned because they can be written by users that are not either the `root` or the current user ([source](http://zsh.sourceforge.net/Doc/Release/Completion-System.html##Use-of-compinit)). For this purpose, remove the rogue permissions:
```sh
compaudit | xargs chmod go-w
```

### Nix Darwin warning regarding `/etc/nix/nix.conf`

This file already exists after first install. Back it up and re-apply nix-darwin:
```shell
sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.bkup
```

See https://github.com/LnL7/nix-darwin/issues/458

### Can't run `p10k configure` as the `/nix/store/` is readonly

Manually change the p10k location and run the command ([source](https://github.com/romkatv/powerlevel10k/issues/967)):
```shell
$ POWERLEVEL9K_CONFIG_FILE=/tmp/setup.p10k p10k configure
```


### No pinentry program when committing

Run the following:
```
pkill gpg-agent
gpg-agent --pinentry-program=/usr/bin/pinentry
```
