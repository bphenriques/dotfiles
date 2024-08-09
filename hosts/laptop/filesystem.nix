{ lib, ... }:
{
  # https://github.com/iynaix/dotfiles/blob/main/recover.sh
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
          # SSD supports 512/512 (physical/logical):
          #   $ lsblk -t /dev/nvme0n1
          # But.. it actually supports 4096/4096:
          #   $ nvme id-ns -H /dev/nvme0n1
          # 512/512 is mostly for compatibility reasons and, in our case, we can set it by setting it up accordingly:
          # $ nvme format --lbaf=1 /dev/nvme0n1
          #
          # https://wiki.archlinux.org/title/Advanced_Format
          # https://www.high-availability.com/docs/ZFS-Tuning-Guide/#alignment-shift-ashiftn
          ashift = "12";
        };

        # https://github.com/KornelJahn/nixos-disko-zfs-test/blob/main/hosts/testhost-disko.nix
        datasets =
          let
            persistConfigLocation = "/persist/config";
            persistCacheLocation = "/persist/cache";

            systemDatasets = {
              system = {
                type = "zfs_fs";
                options.mountpoint = "none";
              };
              "system/root" = {
                type = "zfs_fs";
                # options.mountpoint = "legacy";
                mountpoint = "/";
                postCreateHook = ''zfs snapshot zroot/system/root@blank'';
              };
              "system/nix" = {
                type = "zfs_fs";
                mountpoint = "/nix";
              };
              "system/persist" = {
                type = "zfs_fs";
                mountpoint = "${persistConfigLocation}/system";
              };
              "system/cache" = {
                type = "zfs_fs";
                mountpoint = "${persistCacheLocation}/system";
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
/*
              "home/bphenriques/documents" = {
                type = "zfs_fs";
                mountpoint = "/home/bphenriques/documents";
              };
*/
              "home/bphenriques/persist" = {
                type = "zfs_fs";
                mountpoint = "${persistConfigLocation}/bphenriques";
              };
              "home/bphenriques/cache" = {
                type = "zfs_fs";
                mountpoint = "${persistCacheLocation}/bphenriques";
              };
            };

            dataDatasets = {
              "data" = {
                type = "zfs_fs";
                mountpoint = "/mnt/data";
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
  };

  services.fstrim.enable = true;  # Trim SSD because for some reason is not a default :shrug:

  # Run zramctl to check how good memory is compressed
  zramSwap.enable = true;

  systemd.tmpfiles.settings = {
    # Only accessible by the bphenriques
/*    "grant-bphenriques-permissions" = {
      "/home/bphenriques/documents" = {
        e = {
          user = "bphenriques";
          group = "users";
          mode = "0700";
        };
      };
    };*/
    # Accessible by everyone
    "grant-users-permissions-data" = {
      "/mnt/data" = {
        e = {
          user = "bphenriques";
          group = "users";
          mode = "775";     # Accessible by everyone.
        };
      };
    };
  };
}
