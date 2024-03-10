{ config, lib, pkgs, ... }:
let
  inherit (lib) optional concatMapAttrs;
  homeNetworkAlias = "home-nas";
  homeSambaServer = rec {
    sharedFolder = {
      personal = {
        source = "//${homeNetworkAlias}/bphenriques";
        destination = "/home/${config.user.name}/bphenriques";
      };

      media = {
        source = "//${homeNetworkAlias}/media";
        destination = "/home/${config.user.name}/Media";
      };

      shared = {
        source = "//${homeNetworkAlias}/shared";
        destination = "/home/${config.user.name}/Shared";
      };
    };
  };
in
{
  # Network Disks
  networking.hosts = {
    "192.168.68.53" = [ homeNetworkAlias ];
  };

  # Set Sops secrets and template.
  sops = {
    secrets.samba_server_username = { };
    secrets.samba_server_password = { };
    templates."smb-credentials" = {
      owner = config.user.name;
      content = ''
        username=${config.sops.placeholder.samba_server_username}
        password=${config.sops.placeholder.samba_server_password}
      '';
    };
  };

  environment.systemPackages = [ pkgs.cifs-utils ];
  fileSystems = concatMapAttrs (type: folder: {
    "${folder.destination}" = {
      device = folder.source;
      fsType = "cifs"; # See https://nixos.wiki/wiki/Samba
      options = let
        networkSplitProtectionOpts = "x-systemd.automount,noauto,x-systemd.idle-timeout=15,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
        userOpts = "uid=1000,gid=100";
        credsOpts = "credentials=${config.sops.templates."smb-credentials".path}";
      in ["${networkSplitProtectionOpts},${userOpts},${credsOpts}"];
    };
  }) homeSambaServer.sharedFolder;

  home = {
    xdg.userDirs = {
      enable = true;
      createDirectories = false;  # Do not create any of the folders as they are being mounted.
      documents = "${homeSambaServer.sharedFolder.personal.destination}/paperwork";
      music = "${homeSambaServer.sharedFolder.media.destination}/music";
      pictures = "${homeSambaServer.sharedFolder.personal.destination}/photos";
      videos = "${homeSambaServer.sharedFolder.personal.destination}/videos";
    };
  };
}



