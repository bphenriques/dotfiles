{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.custom.dotfiles;
in
{
  options.custom.dotfiles = {
    enable = mkEnableOption "Whether to enable repository.";
    directory = mkOption {
      type = with lib.types; str;
      description = "Location of the dotfiles repository";
      default = "${config.home.homeDirectory}/.dotfiles";
    };
  };
}
