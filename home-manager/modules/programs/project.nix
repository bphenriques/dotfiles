{ config, lib, pkgs, self, ... }:

with lib;

let
  cfg = config.custom.programs.project;
in {
  options.custom.programs.project = {
    enable = mkEnableOption "project";

    # FIXME: this is not portable
    package = mkOption {
      type = types.package;
      default = self.pkgs.project;
      description = ''
        project package to install.
      '';
    };

    directory = mkOption {
      type = types.str;
      default = config.xdg.userDirs.documents;
      description = ''
        directory where projects live
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
    home.sessionVariables.PROJ_ROOT = cfg.directory;
    programs.fish.interactiveShellInit = mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} --init-shell fish | source
    '';
  };
}