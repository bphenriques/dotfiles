{ pkgs ? (import ./nixpkgs.nix) { } }:
with pkgs;
mkShellNoCC {
  name = "dotfiles";
  meta.description = "Development shell to manage my dotfiles";

  # TODO: more ideas: https://github.com/isabelroses/dotfiles/blob/main/parts/programs/shell.nix#L11
  packages = [
    pkgs.git          # The usual
    pkgs.sops         # Manage secrets
    pkgs.shellcheck   # Scripting sanity checks
    shfmt             # Format shell scripts

    # TODO: Likely format everything except 'hardware-configuration*'
    nixfmt-rfc-style  # Format nix files.

    gnupg
    age
  ];
}