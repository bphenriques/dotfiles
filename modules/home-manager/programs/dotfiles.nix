{ lib, config, self, ... }:

let
  cfg = config.my.dotfiles;
in
{
  options.my.dotfiles = {
    enable = lib.mkEnableOption "dotfiles";
    directory = lib.mkOption {
      type = lib.types.str;
      description = "Location of the dotfiles repository";
      default = "${config.home.homeDirectory}/.dotfiles";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.dotfiles;
      description = "package to install regarding management dotfiles.";
    };

    packageSecrets = lib.mkOption {
      type = lib.types.package;
      default = self.packages.dotfiles-secrets;
      description = "package to install regarding secrets.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package cfg.packageSecrets ];
    home.sessionVariables.DOTFILES_LOCATION = config.my.dotfiles.directory;
  };
}
