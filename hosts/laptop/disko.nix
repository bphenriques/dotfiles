{ lib, inputs, ... }:
{
  disko.devices = {
    disk = {
      vda = {
       type = "disk";
       # Using `by-path` as b/c I won't move the SSDs and `by-id` may change (e.g., `/dev/nvme0n1`).
       # How to: run `ls /dev/disk/by-path/ -l` and cross-reference with `sudo nix run nixpkgs#nvme-cli -- list`
       device = "/dev/disk/by-path/pci-0000:05:00.0-nvme-1";
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
               resumeDevice = false; # Not interested in hibernation.
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
          acltype = "posixacl";
          atime = "off";
        };

        options = {
          # Start with a shell: `nix-shell -p nvme-cli`
          #
          # List SSDs: `sudo nvme list`
          # SSD supports 512/512 (physical/logical): `lsblk -t /dev/nvme0n1`
          # But.. it can support 4096/4096: `sudo nvme id-ns -H /dev/nvme0n1` (difference between "in-use" and "Better")
          # 512/512 is for compatibility and we can increase it: `sudo nvme format --lbaf=1 /dev/nvme0n1`.
          # - Where lbaf corresponds to the number next to "LBA Format"
          #
          # Now let's check again which LBA Format is in-use: `sudo nvme id-ns -H /dev/nvme0n1`
          #
          # Source:
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
          };
          "system/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
          };

          home = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "home/bphenriques" = {
            type = "zfs_fs";
            mountpoint = "/home/bphenriques";
          };
          "home/bphenriques/workdir" = {
            type = "zfs_fs";
            mountpoint = "/mnt/bphenriques";
          };
          games = {
            type = "zfs_fs";
            mountpoint = "/mnt/games";
          };
        };
      };
    };
  };
}
