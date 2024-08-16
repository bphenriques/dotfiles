{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.custom.sunshine;
in
{
  options.custom.sunshine = with types; {
    enable = mkEnableOption "sunshine";
    steamBigPicture = mkEnableOption "Steam Big Picture";
  };

  config = mkIf cfg.enable {
    xdg.configFile = {
      "sunshine/apps.json".text = builtins.toJSON
        {
          env = "/run/current-system/sw/bin";
          apps = lib.optionals cfg.steamBigPicture [
            {
               name = "Steam";
               output = "steam.txt";
               detached = ["${pkgs.util-linux}/bin/setsid ${pkgs.steam}/bin/steam steam://open/bigpicture"];
               image-path = "steam.png";
            }
          ];
        };
    };
  };
}
