{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;

  # --- 1. USER & PATHS ---
  # Ensure the 'user' here has R/W access to the Synology Mounts!
  user = "bphenriques";
  group = "users";

  # Data Roots (Assumed mounted from Synology)
  musicDir = "/mnt/media/music";
  romsDir  = "/mnt/media/emulation";
  backupDir = "/mnt/backups/galaxy-s20";

  # --- 2. DEVICES ---
  devices = {
    galaxy-s20 = {
      id = "AAAAAAA-BBBBBBB-CCCCCCC-DDDDDDD-EEEEEEE-FFFFFFF-GGGGGGG-HHHHHHH";
      name = "Galaxy S20";
      # Optional: Hardcode IP if Local Discovery is flaky on your network
      # addresses = [ "tcp://192.168.1.50:22000" ];
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
  custom.home-server.services.syncthing = {
    port = 8384;
    oidc.enable = true;
  };

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
        "music"        = mkFolder "music" musicDir [ "galaxy-s20" ];
        "roms"         = mkFolder "roms" "${romsDir}/roms" [ "retroid-pocket-5" ];
        "phone_backup" = mkFolder "phone-backup" backupDir [ "galaxy-s20" ];
      };

      gui.theme = "dark";
    };
  };
}