{ pkgs, lib, config, ... }:
{
  services = {
    qbittorrent.enable = true;
    syncthing =
      let
        phone = "Galaxy S23";
      in
      {
        enable = true;
        overrideDevices = true;     # overrides any devices added or deleted through the WebUI
        overrideFolders = true;     # overrides any folders added or deleted through the WebUI
        devices = {
          "${phone}" = { id = "TMPMB7L-ZXHQMWN-I2O3GQI-2E24O5M-ANMBRJV-OREQNRQ-KPKUS2Y-HHHAKAP"; };
        };
        folders = {
          "Music" = {
            path = config.user.musicDir;
            devices = [ phone ];
            type = "sendonly";
          };
        };
      };
  };
}
