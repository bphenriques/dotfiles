# Everything ZFS except the pool layout, which is disko's in ../disko.
{
  boot = {
    supportedFilesystems.zfs = true;
    zfs.forceImportRoot = false;
    # 16GB box: ARC would otherwise take half of RAM and grow, crowding Samba and the nightly rustic run.
    extraModprobeConfig = "options zfs zfs_arc_max=10737418240";
  };

  # ZFS requires it, to guard against importing a pool another host still holds.
  networking.hostId = "192a778f";

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  # Disko declares every dataset mountpoint, so systemd owns the mounts. `zfs mount -a` then races
  # them and loses with "mountpoint or dataset is busy".
  systemd.services.zfs-mount.enable = false;
}
