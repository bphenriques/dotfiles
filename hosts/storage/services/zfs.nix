{ config, lib, ... }:
let
  poolKey = lib.removePrefix "file://" config.disko.devices.zpool.tank.rootFsOptions.keylocation;
in
{
  networking.hostId = "192a778f"; # Required by ZFS to guard against importing a pool another host still holds.
  boot = {
    supportedFilesystems.zfs = true;
    zfs.forceImportRoot = false;
    extraModprobeConfig = "options zfs zfs_arc_max=10737418240"; # 16GB NAS
  };

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  systemd.services.zfs-mount.enable = false; # Disko declares every dataset mountpoint, this would rase and fail.

  systemd.tmpfiles.rules = [ "z ${poolKey} 0400 root root -" ]; # Ensure poolKey permissions do not diverge
}
