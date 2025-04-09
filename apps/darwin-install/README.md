# Setting up new host

1. Installing [`nix` (Determinate Systems)](https://determinate.systems/nix-installer/)
2. Set up a basic flake setup and iterate from there.

# Install

1. Install [`nix` (Determinate Systems)](https://determinate.systems/nix-installer/).
2. Boostrap:
   ```shell
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#darwin-install
   ```

3. Run the post-install:
   ```shell
   HOST=work-macos
   BITWARDEN_EMAIL=me@me.com
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#post-install -- $HOST $BITWARDEN_EMAIL
   ```

4. Apply:
   ```shell
   nix run --extra-experimental-features 'nix-command flakes' github:bphenriques/dotfiles#dotfiles -- sync
   ```

5. Reboot!
