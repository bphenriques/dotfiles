{ lib, ... }:
{
  # TODO: Snapper: https://www.reddit.com/r/NixOS/comments/1bqm7hv/do_you_use_btrfs/
  #      - https://git.jdigi.net/Joseph-DiGiovanni/Nix/src/branch/main/hosts/Joe-Desktop/file-systems.nix
  # TODO: Add these to persist: /var/lib/bluetooth and /var/lib/fprint
  # TODO: https://git.jdigi.net/Joseph-DiGiovanni/Nix/src/branch/main/hosts
  # https://mt-caret.github.io/blog/posts/2020-06-29-optin-state.html
  disko.devices = {
    disk = {
      vda = {
       type = "disk";
       device = "/dev/nvme0n1";
       content = {
         type = "gpt";
         partitions = {
           ESP = {
             size = "1G";
             type = "EF00";
             content = {
               type = "filesystem";
               format = "vfat";
               mountpoint = "/boot"; # TODO: See people setting up options = [ "umask=0077" ]; # Limit access to random seed
             };
           };
           swap = {
             size = "4G";
             content = {
               type = "swap";
               randomEncryption = true;
             };
           };
           root = {
            size = "100%";
            content = {
              type = "btrfs";
              extraArgs = [ "-f" ]; # Override existing partition
              subvolumes = {
                "@" = { };
                "@/root" = {
                  mountpoint = "/";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@/home" = {
                  mountpoint = "/home";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@/nix" = {
                  mountpoint = "/nix";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@/persist" = {
                  mountpoint = "/persist";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@/snapshots" = { # https://wiki.archlinux.org/title/Snapper#Suggested_filesystem_layout
                  mountpoint = "/snapshots";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@/var-log" = {
                  mountpoint = "/var/log";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };

                # Separate volume for things I likely do not want to snapshots
                "@/data" = {
                  mountpoint = "/mnt/data";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                # Prefer to selectively choose what to store
                #"@/var-lib" = {
                #  mountpoint = "/var/lib";
                #  mountOptions = [ "compress=zstd" "noatime" ];
                #};
                # Prefer to selectively choose what to store
                #"@/var-tmp" = {
                #  mountpoint = "/var/tmp";
                #  mountOptions = [ "compress=zstd" "noatime" ];
                #};
              };
            };
           };
          };
        };
      };
    };
  };
}
