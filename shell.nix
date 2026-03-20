{ pkgs }:
pkgs.mkShellNoCC {
  name = "dotfiles";
  meta.description = "Development shell to manage my dotfiles";

  packages = [
    pkgs.git                  # The usual
    pkgs.shellcheck           # Scripting sanity checks
    pkgs.nix-output-monitor   # Better build UI: `nom build` instead of `nix build`
    pkgs.nvd                  # Package changelog: `nvd diff /nix/var/nix/profiles/system-{41,42}-link`
  ];
}
