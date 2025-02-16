{ pkgs }:
pkgs.mkShellNoCC {
  name = "dotfiles";
  meta.description = "Development shell to manage my dotfiles";

  packages = [
    pkgs.git          # The usual
    pkgs.sops         # Manage secrets
    pkgs.shellcheck   # Scripting sanity checks
    pkgs.shfmt        # Format shell scripts
  ];
}
