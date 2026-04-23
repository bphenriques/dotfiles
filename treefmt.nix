{ pkgs, ... }:
{
  projectRootFile = "flake.nix";
  programs = {
    # Formatters
    nixfmt.enable = false;    # Official Nix formatter. Placeholder setup but I am not actually convinced.
    shfmt.enable = true;      # Shell script formatter
    mdformat.enable = true;   # Markdown formatter (no hard wrapping by default)

    # Checks
    shellcheck.enable = true; # Shell script linter
  };

  settings.formatter.shfmt.options = [
    "-i" "2" # 2-space indentation
    "-ci"    # indent switch/case labels
    "-s"     # simplify shell expressions
  ];
}
