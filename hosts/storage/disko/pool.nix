{
  lib,
  private,
  ...
}:
let
  personal = lib.attrNames private.settings.storage.personalShares;
  tankDevices = {
    tank0 = "/dev/disk/by-id/ata-TOSHIBA_HDWG21C_X3H0A04AFP8G";
    tank1 = "/dev/disk/by-id/ata-TOSHIBA_HDWG21C_X3H0A04JFP8G";
  };
  mkPoolDisks = pool: builtins.mapAttrs (_: device: {
    type = "disk";
    inherit device;
    content = {
      type = "gpt";
      partitions.zfs = {
        size = "100%";
        end = "-8G";
        content = {
          type = "zfs";
          inherit pool;
        };
      };
    };
  });
  runtimeMountOptions = [
    "defaults"
    "nofail"
  ];
  mkUserDataset = name: lib.nameValuePair "users/${name}" {
    type = "zfs_fs";
    mountpoint = "/srv/storage/${name}";
    mountOptions = runtimeMountOptions;
  };
  encryptedRootOptions = key: {
    acltype = "posixacl";
    atime = "off";
    canmount = "off";
    casesensitivity = "sensitive";
    compression = "lz4";
    dnodesize = "auto";
    encryption = "aes-256-gcm";
    keyformat = "hex";
    keylocation = "file:///var/lib/zfs/${key}.key";
    mountpoint = "none";
    normalization = "formD";
    snapdir = "hidden";
    utf8only = "on";
    xattr = "sa";
  };
in
{
  disko.devices = {
    disk = mkPoolDisks "tank" tankDevices;

    zpool.tank = {
      type = "zpool";
      mode = "mirror";
      options = {
        ashift = "12";
        autotrim = "off";
        compatibility = "openzfs-2.2";
      };
      rootFsOptions = encryptedRootOptions "tank";
      datasets = {
        users = {
          type = "zfs_fs";
          options = {
            canmount = "off";
            mountpoint = "none";
          };
        };
        shared = {
          type = "zfs_fs";
          mountpoint = "/srv/storage/shared";
          mountOptions = runtimeMountOptions;
        };
        media = {
          type = "zfs_fs";
          mountpoint = "/srv/storage/media";
          mountOptions = runtimeMountOptions;
          options.recordsize = "1M";
        };
        "media/music" = {
          type = "zfs_fs";
          mountpoint = "/srv/storage/media/music";
          mountOptions = runtimeMountOptions;
        };
        "media/books" = {
          type = "zfs_fs";
          mountpoint = "/srv/storage/media/books";
          mountOptions = runtimeMountOptions;
        };
        "media/gaming" = {
          type = "zfs_fs";
          mountpoint = "/srv/storage/media/gaming";
          mountOptions = runtimeMountOptions;
        };
      }
      // lib.listToAttrs (map mkUserDataset personal);
    };
  };
}
