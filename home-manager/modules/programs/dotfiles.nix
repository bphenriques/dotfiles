{ lib, pkgs, config, self, ... }:

# https://codeberg.org/adamcstephens/dotfiles/src/branch/main/home/module.nix#L8
with lib;
let
  cfg = config.custom.dotfiles;
in
{
  options.custom.dotfiles = {
    directory = mkOption {
      type = with lib.types; str;
      description = "Location of the dotfiles repository";
      default = "${config.home.homeDirectory}/.dotfiles";
    };

    # FIXME: this is not portable if anyone tries to import my project
    package = mkOption {
      type = types.package;
      default = self.pkgs.dotfiles;
      description = ''
        proj package to install.
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

  config = {
    home.packages = [ cfg.package ];
    home.sessionVariables.DOTFILES_LOCATION = config.custom.dotfiles.directory;

    programs.fish.interactiveShellInit = mkIf cfg.enableFishIntegration ''
      ${lib.getExe cfg.package} --init-shell fish | source
    '';
  };
}
