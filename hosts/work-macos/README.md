# Work MacOS

Work Macbook Pro using [nix-darwin](https://github.com/nix-darwin/nix-darwin). Keeping it light since there is a balance to be made since I do not own it.

## Install

1. Install Nix with the Lix installer ([nix-darwin prerequisites](https://github.com/nix-darwin/nix-darwin#prerequisites)).
2. Install [Homebrew](https://brew.sh), skipping its "add to PATH" step as `nix-darwin` handles it.
3. Set up the SSH key, repositories, host and secrets:

   ```shell
   nix run github:bphenriques/dotfiles#desktop-post-install -- work-macos <bitwarden-email>
   ```

4. Apply: `nix run ~/.dotfiles#dotfiles -- . sync`.

5. Remove the installer's Nix: `sudo nix-env --profile /nix/var/nix/profiles/default -e lix`.
