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
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted";
                settings = {
                  allowDiscards = true;
                  keyFile = "/tmp/luks-main.key";  # # Plain password
                };
                additionalKeyFiles = [ "/tmp/luks-backup.key" ];  # Stronger password
                content = {
                  type = "btrfs";
                  extraArgs = [ "-L" "nixos" "-f" ]; # override existing partitions

                  # In line with other distros for simplicity. Ephemeral/reproducible data does not need to be snapshoted.
                  subvolumes = {
                    "@root" = {
                      mountpoint = "/";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@var_log" = {
                      mountpoint = "/var/log";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@var_cache" = {
                      mountpoint = "/var/cache";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@var_tmp" = {
                      mountpoint = "/var/tmp";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@nix" = {
                      mountpoint = "/nix";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@swap" = {
                      mountpoint = "/.swapvol";
                      swap.swapfile.size = "13G"; # Dont forget to set the right offset: https://wiki.archlinux.org/title/Power_management/Suspend_and_hibernate#Acquire_swap_file_offset
                    };
                    "@home" = {
                      mountpoint = "/home";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@home/bphenriques" = {
                      mountpoint = "/home/bphenriques";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@home/bphenriques" = {
                      mountpoint = "/home/bphenriques";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@home/bphenriques/workdir" = {
                      mountpoint = "/home/bphenriques/workdir";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                    "@home/bphenriques/games" = {
                      mountpoint = "/home/bphenriques/games";
                      mountOptions = [ "compress=zstd" "noatime" ];
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}