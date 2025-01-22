{ config, lib, pkgs, self, ... }:

let
  cfg = config.custom.programs.project;
in {
  options.custom.programs.project = {
    enable = lib.mkEnableOption "project";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.project;
      description = "package to install.";
    };

    directory = lib.mkOption {
      type = lib.types.str;
      default = config.xdg.userDirs.documents;
      description = "directory where projects live";
    };

    enableFishIntegration = lib.mkOption {
      default = true;
      type = lib.types.bool;
      description = "Whether to enable Fish integration.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.sessionVariables.PROJ_ROOT = cfg.directory;
    programs.fish.interactiveShellInit = lib.mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} --init-shell fish | source
    '';
  };
}