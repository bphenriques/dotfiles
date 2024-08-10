{ lib, ... }:
{
  # ZFS
  networking.hostId = "5b318853";
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
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

  services.fstrim.enable = true;  # Trim SSD because for some reason is not a default :shrug:
  zramSwap.enable = true;         # Run zramctl to check how good memory is compressed

  custom.impermanence = {
    enable = true;
    rootBlankSnapshot = "zroot/system/root@blank";
    configLocation = "/persist/dataConfig/system";
    cacheLocation = "/persist/cache/system";
  };

  # Disko sets boot.loader.grub.devices automatically.
  fileSystems = {
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    "z /mnt/games 0775 root users"                              # Owned by root but can be used by anyone
    "z /home/bphenriques/workdir 0700 bphenriques users 0700"   # Only accessible by bphenriques
  ];
}
