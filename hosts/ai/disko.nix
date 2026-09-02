{ inputs, ... }:
{
  imports = [ inputs.disko.nixosModules.disko ];
  disko.devices.disk.root = {
    type = "disk";
    device = "/dev/disk/by-id/nvme-KINGSTON_OM8TAP42048K1-A00_50026B7384370E36";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          type = "EF00";
          size = "512M";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
            mountOptions = [ "noatime" ];
          };
        };
      };
    };
  };
}
