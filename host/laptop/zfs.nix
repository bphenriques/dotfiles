{ config, lib, ... }:
{
  boot = {
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    supportedFilesystems.zfs = true;
  };

  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  # https://github.com/openzfs/zfs/issues/10891
  #systemd.services.systemd-udev-settle.enable = false;

#  services.sanoid = lib.mkIf cfg.snapshots {
#    enable = true;
#    datasets = {
#      "zroot/persist" = {
#        hourly = 50;
#        daily = 15;
#        weekly = 3;
#        monthly = 1;
#      };
#    };
#  };
}
