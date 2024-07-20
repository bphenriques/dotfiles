# FAQ

#### Fail to find `nix` nor any home-manager binary.

Add the following in your default shell you have the following:
```sh
# Update PATH with Nix and Home-Manager packages
test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
```

Once done, you run the existing commands.

### Nix Darwin warning regarding `/etc/nix/nix.conf`

This file already exists after first install. Back it up and re-apply nix-darwin:
```shell
sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.bkup
```

See https://github.com/LnL7/nix-darwin/issues/458

### No pinentry program when committing

Run the following:
```
pkill gpg-agent
gpg-agent --pinentry-program=/usr/bin/pinentry
```
