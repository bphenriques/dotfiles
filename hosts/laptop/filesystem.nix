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
    dataLocation = "/persist/data/system";
    cacheLocation = "/persist/cache/system";
  };
  custom.home-remote-disks = {
    enable = false;
    smbCredentialsOwnerUsername = "bphenriques";
    uid = 1000;
    guid = 100;
    locations = [
      { mountPoint = "/home/bphenriques/nas"; device = "//home-nas/bphenriques"; }
      { mountPoint = "/mnt/nas-media";        device = "//home-nas/media"; }
      { mountPoint = "/mnt/nas-shared";       device = "//home-nas/shared"; }
    ];
  };

  # Disko sets boot.loader.grub.devices automatically.
  fileSystems = {
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    "z /mnt/games 0775 root users"                              # Owned by root but can be used by any regular user
    "z /home/bphenriques/workdir 0700 bphenriques users 0700"   # Only accessible by bphenriques
  ];
}
