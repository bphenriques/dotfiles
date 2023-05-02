{ pkgs, lib, config, ... }:
{
  services = {
    qbittorrent.enable = true;
    syncthing = {
      enable = true;
      overrideDevices = true;     # overrides any devices added or deleted through the WebUI
      overrideFolders = true;     # overrides any folders added or deleted through the WebUI
      devices = {
        "SM-G980F" = { id = "4ROQS5X-TJG7ROC-JNJFGCF-HE7BXNN-AIAAOH7-63DHRBM-FITGLMN-EFWNUQ4"; };
      };
      folders = {
        "Music" = {
          path = config.user.musicDir;
          devices = [ "SM-G980F" ];
          type = "sendonly";
        };
      };
    };
  };
}
