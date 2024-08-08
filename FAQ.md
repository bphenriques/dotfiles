# FAQ

#### Fail to find `nix` nor any home-manager binary.

Ensure your shell has the following:
```sh
# Update PATH with Nix and Home-Manager packages
test -f "$HOME"/.nix-profile/etc/profile.d/nix.sh && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
```

### No pinentry program when committing

Run the following:
```
pkill gpg-agent
gpg-agent --pinentry-program=/usr/bin/pinentry
```
