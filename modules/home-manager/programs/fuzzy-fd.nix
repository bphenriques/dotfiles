{ config, lib, pkgs, self, ... }:

with lib;

let
  cfg = config.custom.programs.fuzzy-fd;
in {
  options.custom.programs.fuzzy-fd = {
    enable = mkEnableOption "fuzzy-fd";

    # FIXME: this is not portable
    package = mkOption {
      type = types.package;
      default = self.pkgs.fuzzy-fd;
      description = ''
        fuzzy-fd package to install.
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