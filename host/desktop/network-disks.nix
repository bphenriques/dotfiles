{ config, lib, pkgs, ... }:
let
  inherit (builtins) foldl';
  networkFolders = ["Gaming" "Media" "Photos"];
in
{
  # Network Disks
  networking.hosts = {
    "192.168.68.53" = [ "home-nas" ];
  };

  # FIXME: Annoyingly, requires /etc/nixos/smb-secrets to be mounted. See https://nixos.wiki/wiki/Samba
  environment.systemPackages = [ pkgs.cifs-utils ];
  fileSystems =
    foldl' (acc: networkFolder: acc // {
      "/home/${config.user.name}/${networkFolder}" = {
        device = "//home-nas/${networkFolder}";
        fsType = "cifs";
        options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
        in ["${automount_opts},credentials=/etc/nixos/smb-secrets,uid=1000,gid=100"]; # FIXME: Generate this file automatically

        # Or...username=x poassword=y
      };
    }) { } networkFolders;
}



