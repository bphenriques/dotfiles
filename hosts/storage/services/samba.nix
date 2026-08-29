# The SMB view over ../shares; another transport reads the same inventory and its own per-share block.
{ config, lib, ... }:
let
  cfg = config.custom.storage;
in
{
  # Child datasets are already mounted, so they ride in with the plain directories: ownership only.
  selfhost.storage.shares.smb = {
    enable = true;
    openFirewall = true;
    shares = lib.mapAttrs (
      _: share:
      share.smb
      // {
        path = share.root;
        directories = share.childDatasets ++ (share.smb.directories or [ ]);
      }
    ) cfg.shares;
  };

  # Previous Versions in samba's own keys. Whole-name match: catching _daily too matches nothing at all
  # (tested 2026-08-28). sanoid runs under TZ=UTC. media holds no snapshots itself, but its children do.
  services.samba.settings = lib.mapAttrs (_: _: {
    "vfs objects" = "shadow_copy2";
    "shadow:snapdir" = ".zfs/snapshot";
    "shadow:snapdirseverywhere" = "yes";
    "shadow:format" = "autosnap_%Y-%m-%d_%H:%M:%S_hourly";
    "shadow:sort" = "desc";
  }) cfg.shares;

  # Compute runs Prometheus and carries this alert against the scraped units.
  selfhost.monitoring.scopes.smb-shares.enable = false;
}
