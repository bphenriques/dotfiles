{ config, lib, self, ... }:

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
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.sessionVariables.PROJ_ROOT = cfg.directory;
  };
}
