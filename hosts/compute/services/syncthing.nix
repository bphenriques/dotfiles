{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;

  # Ensure the 'user' here has R/W access to the Synology Mounts!
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

  mkFolder = id: path: devices: {
    inherit id path devices;
    type = "sendreceive";
    ignorePerms = true;  # Critical as NAS manages permissions differently.
    versioning = {
      type = "simple";
      params = { keep = "5"; };
    };
  };

in {
  custom.home-server.routes.syncthing = {
    port = 8384;
    oidc.enable = true;
  };

  users.users.syncthing.extraGroups = [
    config.users.groups.homelab-media.name
    config.users.groups.homelab-bphenriques.name
  ];

  services.syncthing = {
    enable = true;
    user = user;
    group = group;
    dataDir = "/var/lib/syncthing";
    configDir = "/var/lib/syncthing/.config/syncthing";

    # Enforce declarative config
    overrideDevices = true;
    overrideFolders = true;

    # Open Firewall for Transfer (22000) & Local Discovery (21027/udp)
    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8384";

    settings = {
      devices = devices;

      # Privacy & Network Tweaks
      options = {
        urAccepted = -1;                # Disable Usage Reporting
        globalAnnounceEnabled = false;  # Disable Global Discovery (Privacy)
        relaysEnabled = false;          # Disable Relays (LAN only)
        natEnabled = false;             # Disable UPnP (since we are local only)
        localAnnounceEnabled = true;    # Ensure devices on the same LAN can find each other
      };

      folders = {
        "music"        = mkFolder "music" paths.media.music.library [ "galaxy-s20" ];
        "roms"         = mkFolder "roms" paths.media.gaming.emulation.roms [ "retroid-pocket-5" ];
        "phone_backup" = mkFolder "phone-backup" paths.bphenriques.backups.phone [ "galaxy-s20" ];
      };

      gui.theme = "dark";
    };
  };
}