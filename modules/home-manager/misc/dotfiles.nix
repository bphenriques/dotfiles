{ lib, pkgs, config, ... }:

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

    graphicalEnvironment = mkOption {
      type = with lib.types; bool;
      description = "If the host contains a graphical environment";
      default = true;
    };
  };

  config = {
    home.packages = with pkgs; [ dotfiles ];
    programs.fish.plugins = [
      { name = "dotfiles"; src = pkgs.fishPlugins.dotfiles.src; }
    ];

    home.sessionVariables.DOTFILES_LOCATION = config.custom.dotfiles.directory;
  };
}
