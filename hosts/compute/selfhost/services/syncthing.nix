{ config, lib, ... }:
let
  serviceCfg = config.selfhost.services.syncthing;
  pathsCfg = config.selfhost.paths;
  selfhostMounts = config.selfhost.storage.smb.mounts;
  syncthingUsers = lib.filterAttrs (_: u: u.services.syncthing.enable) config.selfhost.users;

  # Intentional as select the exact systems to sync
  romSystems = [ "3ds" "dos" "dreamcast" "fbneo" "gb" "gba" "gbc" "megadrive" "snes" "n64" "nds" "nes" "pico8" "ps2" "psp" "psx" "switch" "wii" ];

  allSyncthingDevices = lib.pipe syncthingUsers [
    (lib.mapAttrsToList (_: u: u.services.syncthing.devices))
    lib.flatten
  ];
  userSyncthingDevices = user: syncthingUsers.${user}.services.syncthing.devices;
  toDeviceNames = devices: map (d: d.name) devices;

  mkSendOnlyFolder = id: path: devices: {
    inherit id path;
    devices = toDeviceNames devices;
    type = "sendonly";
    ignorePerms = true;
  };

  mkSyncFolder = id: path: devices: {
    inherit id path;
    devices = toDeviceNames devices;
    type = "sendreceive";
    ignorePerms = true;
  };

  publicFolders = {
    music = mkSendOnlyFolder "music" pathsCfg.media.music.library allSyncthingDevices;
  } // lib.listToAttrs (map (system: lib.nameValuePair "roms-${system}" (
    mkSendOnlyFolder "roms-${system}" "${pathsCfg.media.gaming.emulation.roms}/${system}" allSyncthingDevices
  )) romSystems);

  # Per-user folders
  bphenriquesFolders = lib.optionalAttrs (syncthingUsers ? bphenriques) {
    "bphenriques-phone-backup" = mkSyncFolder "bphenriques-phone-backup" pathsCfg.users.bphenriques.backups.phone (userSyncthingDevices "bphenriques");
    "bphenriques-photos-inbox" = mkSyncFolder "bphenriques-photos-inbox" pathsCfg.users.bphenriques.photos.inbox (userSyncthingDevices "bphenriques");
  };
in
{
  options.selfhost.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.syncthing = {
        enable = lib.mkEnableOption "Syncthing configuration for this user";
        devices = lib.mkOption {
          type = lib.types.listOf (lib.types.submodule {
            options = {
              name = lib.mkOption { type = lib.types.str; };
              id = lib.mkOption { type = lib.types.str; };
            };
          });
          default = [ ];
        };
      };
    });
  };

  config = {
    selfhost = {
      services.syncthing = {
        displayName = "Syncthing";
        description = "File Sync";
        port = 8384;
        healthcheck.path = "/rest/noauth/health";
        access.allowedGroups = [ config.selfhost.groups.admin ];
        forwardAuth.enable = true;
        integrations.homepage.enable = true;
        integrations.homepage.tab = "Admin";
        storage.smb = [ "media" "bphenriques" ];
      };

      runtimeSecrets.syncthing-gui-password = {
        owner = "syncthing";
        restartUnits = [ "syncthing.service" ];
      };
    };

    users.users.syncthing.extraGroups = [
      selfhostMounts.media.group
      selfhostMounts.bphenriques.group
    ];

    services.syncthing = {
      enable = true;
      dataDir = "/var/lib/syncthing";
      configDir = "/var/lib/syncthing/.config/syncthing";

      overrideDevices = true;
      overrideFolders = true;

      openDefaultPorts = false;
      guiAddress = "${serviceCfg.host}:${toString serviceCfg.port}";
      guiPasswordFile = config.selfhost.runtimeSecrets.syncthing-gui-password.path;

      settings = {
        devices = lib.listToAttrs (map (d: lib.nameValuePair d.name { inherit (d) id; }) allSyncthingDevices);
        options = {
          urAccepted = -1;
          crashReportingEnabled = false;
          globalAnnounceEnabled = false;
          relaysEnabled = false;
          natEnabled = false;
          localAnnounceEnabled = true;
        };

        folders = publicFolders // bphenriquesFolders;

        gui = {
          theme = "dark";
          insecureAdminAccess = false;
          insecureSkipHostcheck = true; # Access is constrained by VPN + forwardAuth. Disabling makes syncthing fail.
          user = "admin";
          # credentials provided through guiPasswordFile
        };
      };
    };

    networking.firewall.interfaces.bond0 = {
      allowedTCPPorts = [ 22000 ];
      allowedUDPPorts = [ 21027 22000 ];
    };

    systemd.services.syncthing.serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };
}
