{ lib, ... }:
let
  persistDataLocation = "/persist/data";
  persistCacheLocation = "/persist/cache";
in
{
  disko.devices = {
    disk = {
      vda = {
       type = "disk";
       device = "/dev/nvme0n1";
       content = {
         type = "gpt";

         # Both order and keys are important
         partitions = {
           boot = {
             size = "1M";
             type = "EF02"; # for grub MBR
           };
           ESP = {
             type = "EF00";
             size = "512M";
             content = {
               type = "filesystem";
               format = "vfat";
               mountOptions = [ "umask=0077" ];
               mountpoint = "/boot";
             };
           };
           swap = {
             size = "6G";
             content = {
               type = "swap";
               resumeDevice = false; # I really don't care about hibernation.
               randomEncryption = true;
             };
           };
           zfs = {
             size = "100%";
             content = {
               type = "zfs";
               pool = "zroot";
             };
           };
         };
       };
     };
   };

   zpool = {
     zroot = {
        type = "zpool";

        # https://www.high-availability.com/docs/ZFS-Tuning-Guide/#general-recommendations
        rootFsOptions = {
          compression = "lz4";
          xattr = "sa";
          atime = "off";
        };

        options = {
          # SSD supports 512/512 (physical/logical): `lsblk -t /dev/nvme0n1`
          # But.. it actually supports 4096/4096:    `nvme id-ns -H /dev/nvme0n1`
          # 512/512 is for compatibility and we can increase it: `nvme format --lbaf=1 /dev/nvme0n1`
          #
          # https://wiki.archlinux.org/title/Advanced_Format
          # https://www.high-availability.com/docs/ZFS-Tuning-Guide/#alignment-shift-ashiftn
          ashift = "12";
        };

        datasets = {
          system = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "system/root" = {
            type = "zfs_fs";
            mountpoint = "/";
            postCreateHook = ''zfs snapshot zroot/system/root@blank'';
          };
          "system/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
          };
          "system/data" = {
            type = "zfs_fs";
            mountpoint = "${persistDataLocation}/system";
          };
          "system/cache" = {
            type = "zfs_fs";
            mountpoint = "${persistCacheLocation}/system";
          };
          home = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "home/bphenriques" = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "home/bphenriques/data" = {
            type = "zfs_fs";
            mountpoint = "${persistDataLocation}/bphenriques";
          };
          "home/bphenriques/cache" = {
            type = "zfs_fs";
            mountpoint = "${persistCacheLocation}/bphenriques";
          };
        };
      };
    };
  };
}
