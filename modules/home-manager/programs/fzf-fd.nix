{ config, lib, pkgs, self, ... }:

with lib;
let
  cfg = config.custom.programs.fzf-fd;
in {
  options.custom.programs.fzf-fd = {
    enable = mkEnableOption "fzf-fd";

    # FIXME: this is not portable
    package = mkOption {
      type = types.package;
      default = self.pkgs.fzf-fd;
      description = ''
        fzf-fd package to install.
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