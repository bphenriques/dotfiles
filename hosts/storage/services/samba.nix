# The SMB view over ../shares; another transport reads the same inventory and its own per-share block.
{ config, lib, ... }:
let
  cfg = config.my.storage;
  shares = config.selfhost.storage.shares.smb.shares;

  # Extra doors into a share, for principals that must see only part of it.
  doors = {
    notes = {
      tree = "bphenriques";
      subpath = "notes";
      access.serviceAccounts.machine-agent-alfred-vm = "rw";
    };
    recipes = {
      tree = "media";
      subpath = "recipes";
      access.serviceAccounts.machine-agent-alfred-vm = "rw";
    };
  };
  treeOf = name: doors.${name}.tree or name;
in
{
  selfhost.storage.shares.smb = {
    enable = true;
    openFirewall = true;
    shares =
      lib.mapAttrs (_: share: share.smb // { path = share.root; }) cfg.shares
      // lib.mapAttrs (_: door: {
        path = "${cfg.shares.${door.tree}.root}/${door.subpath}";
        inherit (door) access;
      }) doors;
  };

  # A share's group holds everyone let in through any of its doors.
  users.groups = lib.mapAttrs' (
    tree: _:
    lib.nameValuePair "storage-${tree}" {
      members = lib.unique (
        lib.concatMap (door: lib.attrNames shares.${door}.resolvedAccess) (
          lib.filter (door: treeOf door == tree) (lib.attrNames shares)
        )
      );
    }
  ) cfg.shares;

  services.samba.settings = lib.mkMerge [
    (lib.mapAttrs (name: _: {
      "force group" = "storage-${treeOf name}";
      "create mask" = "0660";
      "directory mask" = "2770";
      "force create mode" = "0660";
      "force directory mode" = "2770";
    }) shares)

    # Previous Versions in samba's own keys. Whole-name match: catching _daily too matches nothing at all
    # (tested 2026-08-28). sanoid runs under TZ=UTC. media holds no snapshots itself, but its children do.
    # streams_xattr: iOS Files mounts a share read-only unless FILE_NAMED_STREAMS is advertised.
    (lib.mapAttrs (_: _: {
      "vfs objects" = "shadow_copy2 streams_xattr";
      "shadow:snapdir" = ".zfs/snapshot";
      "shadow:snapdirseverywhere" = "yes";
      "shadow:format" = "autosnap_%Y-%m-%d_%H:%M:%S_hourly";
      "shadow:sort" = "desc";
    }) cfg.shares)
  ];

  systemd.services.samba-smbd = {
    requires = [ "storage-permissions.service" ];
    after = [ "storage-permissions.service" ];
  };

  # Compute runs Prometheus and carries this alert against the scraped units.
  selfhost.monitoring.scopes.smb-shares.enable = false;
}
