{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.programs.navidrome;
in
{
  options.modules.programs.navidrome = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = mdDoc ''Whether to enable navidrome.'';
    };
  };

  # http://localhost:4533/app/#/song
  # TODO: Automatically setup an admin user?
  # TODO: Be part of home-manager?
  # TODO: Create desktop/menu item
  # TODO: Register server
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # Respect XDG conventions
      (writeScriptBin "navidrome" ''
        #!${stdenv.shell}
        mkdir -p "$XDG_CONFIG_HOME/navidrome"

        ND_CONFIGFILE="$XDG_CONFIG_HOME/navidrome/config.toml" \
          exec ${pkgs.navidrome}/bin/navidrome --datafolder "$XDG_DATA_HOME"
      '')
    ];
  };
}
