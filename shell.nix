{ pkgs ? (import ./nixpkgs.nix) { } }:
with pkgs;
mkShellNoCC {
  name = "dotfiles";
  meta.description = "Development shell to manage my dotfiles";

  # more ideas: https://github.com/isabelroses/dotfiles/blob/main/parts/programs/shell.nix#L11
  # shellHook = config.pre-commit.installationScript;

  DIRENV_LOG_FORMAT = "";

  packages = [
    pkgs.git          # The usual
    pkgs.sops         # Manage secrets
    pkgs.shellcheck   # Scripting sanity checks
    shfmt             # Format shell scripts
    nixfmt-rfc-style  # Format nix files.

    gnupg
    age
    #self.formatter # nix formatter
  ];

  # inputsFrom = [ config.treefmt.build.devShell ];

  #buildInputs = [
  #  nixpkgs-fmt
  #];

  # shellHook = '''';
}