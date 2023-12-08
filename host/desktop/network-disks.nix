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
        source = "//${homeNetworkAlias}/Media";
        destination = "/home/${config.user.name}/Media";
      };

      shared = {
        source = "//${homeNetworkAlias}/Shared";
        destination = "/home/${config.user.name}/Shared";
      };

      server-docker = {
        source = "//${homeNetworkAlias}/docker";
        destination = "/home/${config.user.name}/.docker-nas";
      };
    };

    credentialsFile = "/etc/nixos/smb-secrets"; # FIXME: Use secret manager (e.g. agenix) and do "username=x poassword=y"
    credentialsOpts = "credentials=${homeSambaServer.credentialsFile}";
  };

  # CIFS Opts: See https://nixos.wiki/wiki/Samba
  networkSplitProtectionOpts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
  userOpts = "uid=1000,gid=100";
in
{
  # Network Disks
  networking.hosts = {
    "192.168.68.53" = [ homeNetworkAlias ];
  };

  environment.systemPackages = [ pkgs.cifs-utils ];
  fileSystems = concatMapAttrs (type: folder: {
    "${folder.destination}" = {
      device = folder.source;
      fsType = "cifs";
      options = ["${networkSplitProtectionOpts},${userOpts},${homeSambaServer.credentialsOpts}"];
    };
  }) homeSambaServer.sharedFolder;

  # FIXME: This might need some fine-tuning as the DESKTOP/TEMPLATES/DOWNLOAD/ folders are not created automatically.
  # https://github.com/nix-community/home-manager/blob/master/modules/misc/xdg-user-dirs.nix
  home = {
    xdg.userDirs = {
      enable = true;
      createDirectories = false;  # Do not create any of the folders as they are being mounted.
      documents = "${homeSambaServer.sharedFolder.personal.destination}/Documents";
      music = "${homeSambaServer.sharedFolder.media.destination}/Music";
      pictures = "${homeSambaServer.sharedFolder.personal.destination}/Photos";
      videos = "${homeSambaServer.sharedFolder.personal.destination}/Videos";
    };
  };

  system.userActivationScripts.checkHomeNasCredentialsFile = ''
    if [ ! -f "${homeSambaServer.credentialsFile}" ]; then
      echo "WARNING: The credentials file '${homeSambaServer.credentialsFile}' does not exist!"
    fi
  '';
}



