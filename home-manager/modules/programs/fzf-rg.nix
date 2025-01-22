{ config, lib, pkgs, self, ... }:

let
  cfg = config.custom.programs.fzf-rg;
in {
  options.custom.programs.fzf-rg = {
    enable = lib.mkEnableOption "fzf-rg";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.fzf-rg;
      description = "package to install.";
    };

    enableFishIntegration = lib.mkOption {
      default = true;
      type = lib.types.bool;
      description = "Whether to enable Fish integration.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    programs.fish.interactiveShellInit = lib.mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} --init-shell fish | source
    '';
  };
}