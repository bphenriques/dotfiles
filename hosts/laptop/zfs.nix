{ lib, pkgs, config, ... }:
let
  groups = config.users.groups;
  users = config.users.users;
in
{
  networking.hostId = "5b318853";
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  services.sanoid = {
    enable = true;
    datasets."home/bphenriques/workdir" = {
      autosnap = true;
      autoprune = true;
      frequent_period = 30; # Every half-hour.
      hourly = 24;          # keep hourly snapshots for the last day
      daily = 12;           # keep 12 snapshots per day before that
      weekly = 7;           # keep 7 weekly snapshots
      monthly = 1;          # keep 1 backup per month
      yearly = 0;           # snapshots older that 1 year should have already been commited or backed up
    };
  };

  systemd.tmpfiles.rules = [
    "z /mnt/games             0775 root                        ${groups.users.name}"
    "z /mnt/bphenriques       0700 ${users.bphenriques.name}   ${groups.users.name}"
  ];
}
