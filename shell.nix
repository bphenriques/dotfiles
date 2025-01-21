{ pkgs ? (import ./nixpkgs.nix) { } }:
pkgs.mkShellNoCC {
  name = "dotfiles";
  meta.description = "Development shell to manage my dotfiles";

  packages = with pkgs; [
    git          # The usual
    sops         # Manage secrets
    shellcheck   # Scripting sanity checks
    shfmt        # Format shell scripts
  ];
}
