{ pkgs, ... }:
{
  projectRootFile = "flake.nix";
  programs = {
    # Formatters
    nixfmt.enable = true;     # Official Nix formatter
    shfmt.enable = true;      # Shell script formatter

    # Checks
    shellcheck.enable = true; # Shell script linter
  };

  settings.formatter.shfmt.options = [ "-i" "2" "-ci" "-s" ]; # 2-space indent, indent case labels, simplify var
}
