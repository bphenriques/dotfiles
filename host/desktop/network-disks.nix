{ config, lib, pkgs, ... }:
let
  inherit (builtins) foldl';
  networkFolders = ["Books" "Documents" "Gaming" "Photos" "Music"];

  # CIFS Opts: See https://nixos.wiki/wiki/Samba
  networkSplitProtectionOpts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
  credentialsOpts = "credentials=/etc/nixos/smb-secrets"; # FIXME: Use secret manager (e.g. agenix) and do "username=x poassword=y"
  userOpts = "uid=1000,gid=100";
in
{
  # Network Disks
  networking.hosts = {
    "192.168.68.53" = [ "home-nas" ];
  };

  environment.systemPackages = [ pkgs.cifs-utils ];
  fileSystems =
    foldl' (acc: networkFolder: acc // {
      "/home/${config.user.name}/${networkFolder}" = {
        device = "//home-nas/${networkFolder}";
        fsType = "cifs";
        options = ["${networkSplitProtectionOpts},${credentialsOpts},${userOpts}"];
      };
    }) { } networkFolders;

  home = {
    xdg.userDirs = {
      enable = true;
      createDirectories = false;
      documents = "/home/${config.user.name}/Documents";
      music = "/home/${config.user.name}/Music";
      pictures = "/home/${config.user.name}/Photos";
    };
  };
}



