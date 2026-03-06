{ config, lib, ... }:
let
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.cifs.mounts;
  syncthingUsers = config.custom.homelab.enabledUsers.syncthing;

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

  # Per-user folders - hardcoded for now as paths.bphenriques is user-specific.
  # TODO: Generalize when adding more users (requires per-user paths in paths.nix)
  bphenriquesFolders = lib.optionalAttrs (syncthingUsers ? bphenriques) {
    bphenriques-phone-backup = mkSyncFolder "bphenriques-phone-backup" pathsCfg.bphenriques.backups.phone (userSyncthingDevices "bphenriques");
    bphenriques-photos-inbox = mkSyncFolder "bphenriques-photos-inbox" pathsCfg.bphenriques.photos.inbox (userSyncthingDevices "bphenriques");
  };
in
{
  custom.homelab.services.syncthing = {
    port = 8384;
    forwardAuth.enable = true;
    integrations.homepage = {
      enable = true;
      category = "Admin";
      description = "File Sync";
    };
  };

  users.users.syncthing.extraGroups = [
    homelabMounts.media.group
    homelabMounts.bphenriques.group
  ];
  custom.homelab.cifs.mounts = {
    media.systemd.dependentServices = [ "syncthing" ];
    bphenriques.systemd.dependentServices = [ "syncthing" ];
  };

  services.syncthing = {
    enable = true;
    dataDir = "/var/lib/syncthing";
    configDir = "/var/lib/syncthing/.config/syncthing";

    overrideDevices = true;
    overrideFolders = true;

    openDefaultPorts = true;
    guiAddress = "127.0.0.1:8384";

    settings = {
      devices = lib.listToAttrs (map (d: lib.nameValuePair d.name { id = d.id; }) allSyncthingDevices);
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
        insecureAdminAccess = true; # FIXME: this is insecure as compromised services will be access this interface
      };
    };
  };

  systemd.services.syncthing.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };
}
