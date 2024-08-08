{ config, lib, pkgs, ... }:

{
  xdg.configFile."sunshine/apps.json".text = builtins.toJSON
    {
      env = "/run/current-system/sw/bin";
      apps = [
        {
           name = "Steam";
           output = "steam.txt";
           detached = ["${pkgs.util-linux}/bin/setsid ${pkgs.steam}/bin/steam steam://open/bigpicture"];
           image-path = "steam.png";
        }
      ];
    };
}
