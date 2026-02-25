{ config, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  user = "bphenriques";
  group = "users";

  devices = {
    galaxy-s20 = {
      id = "AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD-EEEEEEE-FFFFFFF-GGGGGGG-HHHHHHH";
      name = "Galaxy S20";
    };
    retroid-pocket-5 = {
      id = "ZZZZZZZ-YYYYYYY-XXXXXXX-WWWWWWW-VVVVVVV-UUUUUUU-TTTTTTT-SSSSSSS";
      name = "Retroid Pocket 5";
    };
  };

  mkFolder = id: path: deviceList: {
    inherit id path;
    devices = deviceList;
    type = "sendreceive";
    ignorePerms = true;
    versioning = {
      type = "simple";
      params = { keep = "5"; };
    };
  };
in
{
  custom.homelab.services.syncthing = {
    port = 8384;
    forwardAuth.enable = true;
    dashboard = {
      enable = true;
      category = "Admin";
      description = "File Sync";
      icon = "syncthing.svg";
    };
  };

  users.users.syncthing.extraGroups = [
    homelabMounts.media.group
    homelabMounts.bphenriques.group
  ];
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "syncthing" ];
  custom.fileSystems.homelab.mounts.bphenriques.systemd.dependentServices = [ "syncthing" ];

  services.syncthing = {
    enable = true;
    inherit user group;
    dataDir = "/var/lib/syncthing";
    configDir = "/var/lib/syncthing/.config/syncthing";

    overrideDevices = true;
    overrideFolders = true;

    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8384";

    settings = {
      inherit devices;

      options = {
        urAccepted = -1;
        globalAnnounceEnabled = false;
        relaysEnabled = false;
        natEnabled = false;
        localAnnounceEnabled = true;
      };

      folders = {
        "music"        = mkFolder "music" pathsCfg.media.music.library [ "galaxy-s20" ];
        "roms"         = mkFolder "roms" pathsCfg.media.gaming.emulation.roms [ "retroid-pocket-5" ];
        "phone_backup" = mkFolder "phone-backup" pathsCfg.bphenriques.backups.phone [ "galaxy-s20" ];
      };

      gui.theme = "dark";
    };
  };

  systemd.services.syncthing = {
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };
}
