{ config, lib, pkgs, ... }:

with lib;
let
  inherit (lib) optional concatMapAttrs;
  cfg = config.dotfiles.profiles.home-network-disk;
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
  options.dotfiles.profiles.home-network-disk = with types; {
    enable = mkOption {
      type = bool;
      default = false;
      description = mdDoc ''Whether to set-up home's network disk.'';
    };

    ip = mkOption {
      type = str;
      default = null;
      description = mdDoc ''Ip of the network disk'';
    };

    personalDir = mkOption {
      type = nullOr str;
      default = null;
      description = mdDoc ''Path to personal network directory'';
    };

    mediaDir = mkOption {
      type = nullOr str;
      default = null;
      description = mdDoc ''Path to media network directory'';
    };

    sharedDir = mkOption {
      type = nullOr str;
      default = null;
      description = mdDoc ''Path to shared network directory'';
    };
  };

  config = mkIf cfg.enable {
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


#    networking.hosts = {
#      "192.168.68.53" = [ homeNetworkAlias ];
#    };
#
#    # Set Sops secrets and template.
#    sops = {
#      secrets.samba_server_username = { };
#      secrets.samba_server_password = { };
#      templates."smb-credentials" = {
#        owner = config.user.name;
#        content = ''
#          username=${config.sops.placeholder.samba_server_username}
#          password=${config.sops.placeholder.samba_server_password}
#        '';
#      };
#    };
#    home = {
#      xdg.userDirs = {
#        enable = true;
#        createDirectories = false;  # Do not create any of the folders as they are being mounted.
#        documents = "${homeSambaServer.sharedFolder.personal.destination}/paperwork";
#        music = "${homeSambaServer.sharedFolder.media.destination}/music";
#        pictures = "${homeSambaServer.sharedFolder.personal.destination}/photos";
#        videos = "${homeSambaServer.sharedFolder.personal.destination}/videos";
#      };
#    };
  };
}
