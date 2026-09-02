{
  lib,
  private,
  ...
}:
let
  runtimeMountOptions = [
    "defaults"
    "nofail"
  ];

  mkTankDisk = device: {
    type = "disk";
    inherit device;
    content = {
      type = "gpt";
      partitions.zfs = {
        # Short of the end on purpose: a mirror member must not be smaller than the vdev, and
        # nominally identical drives differ by a few MB. 8 GiB verified free on both spindles.
        end = "-8G";
        content = {
          type = "zfs";
          pool = "tank";
        };
      };
    };
  };

  mkDataset = mountpoint: {
    type = "zfs_fs";
    inherit mountpoint;
    mountOptions = runtimeMountOptions;
  };

  personalDatasets = lib.mapAttrs' (
    name: _: lib.nameValuePair "users/${name}" (mkDataset "/srv/storage/${name}")
  ) private.settings.storage.personalShares;
in
{
  disko.devices = {
    disk = builtins.mapAttrs (_: mkTankDisk) {
      tank0 = "/dev/disk/by-id/ata-TOSHIBA_HDWG21C_X3H0A04AFP8G";
      tank1 = "/dev/disk/by-id/ata-TOSHIBA_HDWG21C_X3H0A04JFP8G";
    };

    zpool.tank = {
      type = "zpool";
      mode = "mirror";
      options = {
        ashift = "12";
        autotrim = "off";
        compatibility = "openzfs-2.2";
      };
      rootFsOptions = {
        acltype = "posixacl";
        atime = "off";
        canmount = "off";
        casesensitivity = "sensitive";
        compression = "lz4";
        dnodesize = "auto";
        encryption = "aes-256-gcm";
        keyformat = "hex";
        keylocation = "file:///var/lib/zfs/tank.key";
        mountpoint = "none";
        normalization = "formD";
        snapdir = "hidden";
        utf8only = "on";
        xattr = "sa";
      };
      datasets = {
        users = {
          type = "zfs_fs";
          options = {
            canmount = "off";
            mountpoint = "none";
          };
        };
        shared = mkDataset "/srv/storage/shared";
        media = mkDataset "/srv/storage/media" // { options.recordsize = "1M"; };
        "media/music" = mkDataset "/srv/storage/media/music";
        "media/books" = mkDataset "/srv/storage/media/books";
        "media/gaming" = mkDataset "/srv/storage/media/gaming";
      }
      // personalDatasets;
    };
  };
}
