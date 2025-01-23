{ lib, pkgs, config, self, ... }:

# https://codeberg.org/adamcstephens/dotfiles/src/branch/main/home/module.nix#L8
let
  cfg = config.custom.dotfiles;
in
{
  options.custom.dotfiles = {
    directory = lib.mkOption {
      type = lib.types.str;
      description = "Location of the dotfiles repository";
      default = "${config.home.homeDirectory}/.dotfiles";
    };

    # FIXME: this is not portable if anyone tries to import my project
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.dotfiles;
      description = "package to install.";
    };

    enableFishIntegration = lib.mkOption {
      default = true;
      type = lib.types.bool;
      description = "Whether to enable Fish integration.";
    };
  };

  config = {
    home.packages = [ cfg.package ];
    home.sessionVariables.DOTFILES_LOCATION = config.custom.dotfiles.directory;
    programs.fish.interactiveShellInit = lib.mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} --init-shell fish | source
    '';
  };
}
