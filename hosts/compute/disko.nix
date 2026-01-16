_:
{
  disko.devices = {
    disk = {
      vda = {
        type = "disk";
        device = "/dev/disk/by-uuid/1E22-325C";
        content = {
          type = "gpt";

          # Both order and keys are important
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
              };
            };
          };
        };
      };
    };
  };
}
