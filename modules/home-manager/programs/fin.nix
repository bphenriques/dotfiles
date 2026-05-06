{ config, lib, pkgs, self, ... }:

let
  cfg = config.custom.programs.fin;
in {
  options.custom.programs.fin = {
    enable = lib.mkEnableOption "fin";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.fin;
      description = "fin package to install.";
    };

    directory = lib.mkOption {
      type = lib.types.str;
      description = "file or directory with <!-- fin:* --> budget tables.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      cfg.package
      pkgs.hledger
      pkgs.hledger-ui
      pkgs.puffin
    ];
    home.sessionVariables.FIN_DIR = cfg.directory;
  };
}
