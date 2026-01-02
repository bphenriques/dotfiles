_:
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
