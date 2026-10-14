# Work MacOS

Work Macbook Pro using [nix-darwin](https://github.com/nix-darwin/nix-darwin). Keeping it light since there is a balance to be made since I do not own it.

## Install

1. Install Nix with the Lix installer ([nix-darwin prerequisites](https://github.com/nix-darwin/nix-darwin#prerequisites)).
2. Install [Homebrew](https://brew.sh), skipping its "add to PATH" step as `nix-darwin` handles it.
3. Add an SSH key to GitHub, then:

   ```shell
   git clone git@github.com:bphenriques/dotfiles.git ~/.dotfiles
   echo work-macos > ~/.dotfiles/.nix-host
   nix run ~/.dotfiles#dotfiles -- . sync
   ```

4. Remove the installer's Nix: `sudo nix-env --profile /nix/var/nix/profiles/default -e lix`.
