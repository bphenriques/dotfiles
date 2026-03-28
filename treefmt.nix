{ pkgs, ... }:
{
  projectRootFile = "flake.nix";
  programs = {
    # Formatters
    nixfmt.enable = true;     # Official Nix formatter (used by nixpkgs)
    shfmt.enable = true;      # Shell script formatter

    # Checks
    shellcheck.enable = true; # Shell script linter
  };

  settings.formatter.shfmt.options = [
    "-i" "2" # 2-space indentation
    "-ci"    # indent switch/case labels
    "-s"     # simplify shell expressions
  ];
}
