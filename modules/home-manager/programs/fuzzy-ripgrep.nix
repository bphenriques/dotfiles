{ config, lib, pkgs, self, ... }:

with lib;

let
  cfg = config.custom.programs.fuzzy-ripgrep;
in {
  options.custom.programs.fuzzy-ripgrep = {
    enable = mkEnableOption "fuzzy-ripgrep";

    # FIXME: this is not portable
    package = mkOption {
      type = types.package;
      default = self.pkgs.fuzzy-ripgrep;
      description = ''
        fuzzy-ripgrep package to install.
      '';
    };

    enableFishIntegration = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether to enable Fish integration.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    programs.fish.interactiveShellInit = mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} --init-shell fish | source
    '';
  };
}