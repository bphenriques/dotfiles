{ lib, pkgs, config, ... }:

# TODO: https://github.com/malob/nixpkgs/blob/master/home/gh-aliases.nix#L2
# TODO: https://github.com/Misterio77/nix-config/blob/cdc35ca281891268c6e9772cca1e66fb39de04ab/home/misterio/features/cli/git.nix
# TODO: https://github.com/jordanisaacs/dotfiles/blob/master/scripts/default.nix#L130
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
  };
}
