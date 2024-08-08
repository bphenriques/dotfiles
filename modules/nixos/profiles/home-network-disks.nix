{ config, lib, pkgs, ... }:

with lib;
let
  inherit (lib) optional concatMapAttrs;
  cfg = config.custom.home-remote-disks;

  folderOpts = {
    mountPoint = mkOption {
      type = str;
      description = mdDoc ''Where the remote folder should be mounted locally.'';
    };
    device = mkOption {
      type = str;
      description = mdDoc ''The address of the remote folder.'';
    };
  };

  mkHomeServerCifsFs = mountPoint: remoteFolder: let
    networkSplitProtectionOpts = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=15"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
    ];
    userOpts = "uid=${cfg.uid},gid=${cfg.guid}";
    credsOpts = "credentials=${config.sops.templates."smb-credentials".path}";
  in
  {
    mountPoint = {
       device = remoteFolder;
       fsType = "cifs"; # See https://nixos.wiki/wiki/Samba
       options = builtins.concatStringsSep "," userOpts ++ credsOpts ++ networkSplitProtectionOpts;
    };
  };
in
{
  options.custom.home-remote-disks = with lib.types; {
    enable = mkEnableOption "Whether to set-up home's network disk.";

    smbCredentialsOwnerUsername = mkOption {
      type = str;
      description = mdDoc ''Who owns the secret template file'';
    };

    uid = mkOption {
      type = int;
      description = mdDoc ''User id that mounts the CIFS. Use config.users.users.<username>.uid if explicitly set'';
    };

    guid = mkOption {
      type = int;
      description = mdDoc ''Group id that mounts the CIFS. Use config.users.users.<username>.guid if explicitly set'';
    };

    locations = mkOption {
      type = listOf folderOpts;
      default = [];
    };
  };

  config = mkIf (cfg.enable && cfg.folders != []) {
    environment.systemPackages = [ pkgs.cifs-utils ];
    sops = {
      secrets = {
        samba_server_username = { };
        samba_server_password = { };
      };
      templates."smb-credentials" = {
        owner = cfg.smbCredentialsOwnerUsername;
        content = ''
          username=${config.sops.placeholder.samba_server_username}
          password=${config.sops.placeholder.samba_server_password}
        '';
      };
    };

    fileSystems = builtins.foldl'
      (res: location: res // (mkHomeServerCifsFs location.mountPoint location.device))
      { }
      cfg.locations;
  };
}
