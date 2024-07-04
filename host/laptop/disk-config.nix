{ lib, ... }:
{
  # https://mt-caret.github.io/blog/posts/2020-06-29-optin-state.html


  # https://github.com/Prometheus7435/nix-config/blob/main/nixos/phoenix/disks.nix
  # https://github.com/Prometheus7435/nix-config/blob/main/nixos/odyssey/disks.nix
  # https://grahamc.com/blog/nixos-on-zfs/
  # https://www.reddit.com/r/NixOS/comments/lvegkr/nixos_zfs_dataset_layout_questions/
  # https://github.com/bhougland18/nixos_config
  # https://jrs-s.net/2018/08/17/zfs-tuning-cheat-sheet/
  # https://grahamc.com/blog/nixos-on-zfs/
  # https://nixos.wiki/wiki/ZFS
  # https://grahamc.com/blog/erase-your-darlings/
  # https://www.reddit.com/r/NixOS/comments/11o5vgp/manage_zfs_pools_in_nixos/
  # https://www.reddit.com/r/NixOS/comments/1ad0m5n/impermanence_disko_setup/
  # https://github.com/iynaix/dotfiles/blob/main/recover.sh
  # https://github.com/iynaix/dotfiles/blob/main/nixos/zfs.nix

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
               mountpoint = "/boot"; # TODO: See people setting up options = [ "umask=0077" ]; # Limit access to random seed
             };
           };
           swap = {
             size = "6G";
             content = {
               type = "swap";
               resumeDevice = false; # I really don't care about hibernation.
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
        # SSD supports 512/512 (physical/logical):
        #   $ lsblk -t /dev/nvme0n1
        #
        # But.. it actually supports 4096/4096:
        #   $ nvme id-ns -H /dev/nvme0n1
        #
        # 512/512 is mostly for compatability reasons and, in our case, we can set it by setting it up accordingly:
        # $ nvme format --lbaf=1 /dev/nvme0n1
        #
        # https://wiki.archlinux.org/title/Advanced_Format
        # https://www.high-availability.com/docs/ZFS-Tuning-Guide/#alignment-shift-ashiftn
        ashift = "12";
      };

      # https://github.com/KornelJahn/nixos-disko-zfs-test/blob/main/hosts/testhost-disko.nix
      datasets =
        let
          systemDatasets = {
            system = {
              type = "zfs_fs";
              options.mountpoint = "none";
            };
            "system/root" = {
              type = "zfs_fs";
              mountpoint = "/";
              options.mountpoint = "legacy";
              postCreateHook = ''zfs snapshot zroot/system/root@blank'';
            };
            "system/nix" = {
              type = "zfs_fs";
              mountpoint = "/nix";
              options.mountpoint = "legacy";
            };
            "system/persist" = {
              type = "zfs_fs";
              mountpoint = "/persist/system";
              options.mountpoint = "legacy";
            };
            "system/cache" = {
              type = "zfs_fs";
              mountpoint = "/persist/system/cache";
              options.mountpoint = "legacy";
            };
          };

          homeDatasets = {
            home = {
              type = "zfs_fs";
              options.mountpoint = "none";
            };
            "home/bphenriques" = {
              type = "zfs_fs";
              options.mountpoint = "none";
            };
            "home/bphenriques/documents" = {
              type = "zfs_fs";
              mountpoint = "/home/bphenriques/documents";
              options.mountpoint = "legacy";
            };
            "home/bphenriques/persist" = {
              type = "zfs_fs";
              mountpoint = "/persist/bphenriques";
              options.mountpoint = "legacy";
            };
            "home/bphenriques/cache" = {
              type = "zfs_fs";
              mountpoint = "/persist/bphenriques/cache";
              options.mountpoint = "legacy";
            };
          };

          dataDatasets = {
            "data" = {
              type = "zfs_fs";
              mountpoint = "/mnt/data";
              options.mountpoint = "legacy";
            };
          };

        in systemDatasets // homeDatasets // dataDatasets;
    };
   };
  };

  fileSystems = {
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;
    "/persist/system".neededForBoot = true;
    "/persist/system/cache".neededForBoot = true;
    "/persist/bphenriques".neededForBoot = true;
    "/persist/bphenriques/cache".neededForBoot = true;
  };
}
