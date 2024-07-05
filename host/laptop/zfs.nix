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

#  boot.initrd.systemd.services.initrd-rollback-root = {
#    after = [ "zfs-import-rpool.service" ];
#    requires = [ "zfs-import-rpool.service" ];
#    before = [
#      "sysroot.mount"
#      "local-fs.target"
#    ];
#    description = "Rollback root fs";
#    serviceConfig = {
#      Type = "oneshot";
#      ExecStart = "${config.boot.zfs.package}/sbin/zfs rollback -r rpool/local/root@blank";
#    };
#  };
