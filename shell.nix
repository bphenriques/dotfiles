{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShellNoCC {
  name = "dotfiles";
  meta.description = "Development shell to manage my dotfiles";

  # more ideas: https://github.com/isabelroses/dotfiles/blob/main/parts/programs/shell.nix#L11
  # shellHook = config.pre-commit.installationScript;

  DIRENV_LOG_FORMAT = "";

  packages = [
    pkgs.git    # Required by flakes
    pkgs.sops   # Required to manage secrets
    #self.formatter # nix formatter
    # inputs'.agenix.packages.agenix # secrets
    # inputs'.deploy-rs.packages.deploy-rs # remote deployment
  ];

  # inputsFrom = [ config.treefmt.build.devShell ];

  #buildInputs = [
  #  nixpkgs-fmt
  #];

  shellHook = ''
    echo "> .dotfiles development shell"
  '';
}