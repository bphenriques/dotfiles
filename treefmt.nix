{ pkgs, lib, ... }:
{
  projectRootFile = "flake.nix";
  programs = {
    # Formatters
    nixfmt.enable = false;                           # Official Nix formatter. Disabled until I am convinced.
    shfmt.enable = true;                             # Shell script formatter
    mdformat.enable = true;                          # Markdown formatter
    mdformat.plugins = ps: [ ps.mdformat-gfm ];      # GitHub Flavored Markdown (tables, task lists, strikethrough)
    mdformat.settings.number = true;                 # Preserve consecutive numbering in ordered lists
    fish_indent.enable = true;                       # Fish shell formatter

    # Checks
    shellcheck.enable = true;                        # Shell script linter
    shellcheck.severity = "warning";                 # Ignore info-level hints (e.g., SC1091)
    deadnix.enable = true;                           # Detect unused Nix code
    deadnix.no-lambda-pattern-names = true;          # Skip NixOS/HM module args (e.g., { pkgs, lib, ... })
    deadnix.priority = 1;                            # Run deadnix before statix
    statix.enable = true;                            # Nix anti-pattern linter
    statix.priority = 2;
  };

  settings.formatter = {
    # Follow Google's Shell Style Guide: https://google.github.io/styleguide/shellguide.html
    shfmt.options = [
      "-i"
      "2"   # 2-space indentation
      "-ci" # indent switch/case labels
      "-bn" # binary ops (&&, ||, |) may start a line
      "-s"  # simplify shell expressions
    ];
    shellcheck.excludes = [
      ".envrc" # is a direnv file, not a regular shell script.
      # Avoids false positive related with lack of shebang.
      "hosts/**/backup.sh"
      "packages/**/script.sh"
      "apps/**/script.sh"
    ];

    # Nushell formatter (not yet in treefmt-nix)
    nufmt = let config = pkgs.writeText "nufmt.nuon" "{ indent: 2, line_length: 120 }";
    in {
      command = lib.getExe pkgs.nufmt;
      options = [ "--config" (toString config) ];
      includes = [ "*.nu" ];
    };
  };
}
