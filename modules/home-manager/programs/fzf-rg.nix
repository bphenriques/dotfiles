{ config, lib, pkgs, self, ... }:

with lib;
let
  cfg = config.custom.programs.fzf-rg;
in {
  options.custom.programs.fzf-rg = {
    enable = mkEnableOption "fzf-rg";

    # FIXME: this is not portable
    package = mkOption {
      type = types.package;
      default = self.pkgs.fzf-rg;
      description = ''
        fzf-rg package to install.
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