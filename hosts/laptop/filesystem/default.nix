{ lib, pkgs, config, ... }:
let
  groups = config.users.groups;
  users = config.users.users;

  bphenriquesPersistData = config.home-manager.users.bphenriques.custom.impermanence.dataLocation;
  bphenriquesPersistCache = config.home-manager.users.bphenriques.custom.impermanence.cacheLocation;
in
{
  imports = [
    ./disko.nix           # Sets the partitions
    ./network-drives.nix  # Adds additional network drives
  ];

  # ZFS
  networking.hostId = "5b318853";
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  #system/data
  #home/bphenriques/workdir
  #home/bphenriques/data
    # https://github.com/prescientmoon/everything-nix/blob/5247160b4367f37a775d7278a52412f3cb0886ac/hosts/nixos/lapetus/services/zfs.nix#L10
    #  services.sanoid = lib.mkIf cfg.snapshots {
    #    enable = true;
    #    datasets = {
    #      "zroot/persist" = {
    #        hourly = 50;
    #        daily = 15;
    #        weekly = 3;
    #        monthly = 1;
    #      };
    #    };
    #  };

  services.fstrim.enable = true;  # Trim SSD because it is not set by default :shrug:
  zramSwap.enable = true;         # Run zramctl to check how good memory is compressed

  custom.impermanence = {
    enable = true;
    rootBlankSnapshot = "zroot/system/root@blank";
    dataLocation = "/persist/data/system";
    cacheLocation = "/persist/cache/system";
  };

  fileSystems = {
    # Disko sets boot.loader.grub.devices automatically.
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;

    # ZFS Mounts
    "${bphenriquesPersistData}".neededForBoot = true;
    "${bphenriquesPersistCache}".neededForBoot = true;
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    # Fix permissions regarding root user
    "z ${config.sops.age.keyFile} 0700 root root" # nixos-anywhere sets the wrong permissions

    # Review ZFS datasets permissions
    "z /mnt/games                   0775 root                        ${groups.users.name}"
    "z /mnt/bphenriques             0700 ${users.bphenriques.name}   ${groups.users.name}"
    "z ${bphenriquesPersistData}    0700 ${users.bphenriques.name}   ${groups.users.name}"
    "z ${bphenriquesPersistCache}   0700 ${users.bphenriques.name}   ${groups.users.name}"
  ];
}
