1. Install [`nix` (Determinate Systems)](https://determinate.systems/nix-installer/).
2. Boostrap:
   ```shell
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```

3. Run the post-install:
   ```shell
   HOST=work-macos
   BITWARDEN_EMAIL=me@me.com
   nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#post-install" -- $HOST $BITWARDEN_EMAIL
   ```

5. Apply:
   ```shell
   nix run --extra-experimental-features 'nix-command flakes' "github:bphenriques/dotfiles#dotfiles" -- sync
   ```

6. Reboot!
