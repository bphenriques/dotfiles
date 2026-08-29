{
  config,
  lib,
  private,
  ...
}:
let
  cfg = config.custom.storage;

  personal = private.settings.storage.personalShares;

  household = {
    shared = {
      dataset = "shared";
      backup = true;
      smb = {
        owner = "bphenriques";
        gid = 989;
        access = {
          groups.${private.groups.users} = "rw";
          users.machine-compute = "rw";
        };
      };
    };
    media = {
      dataset = "media";
      backup = true;
      snapshots = false;
      childDatasets = [
        "music"
        "books"
        "gaming"
      ];
      smb = {
        owner = "bphenriques";
        gid = 990;
        directories = [
          "movies"
          "tv"
          "downloads"
          "downloads/incomplete"
        ]
        # The arrs health-check their category dir before any download would create it.
        ++ map (c: "downloads/${c}") (lib.attrValues config.custom.fleet.media.downloadCategories);
        access = {
          groups.${private.groups.users} = "rw";
          users = {
            machine-compute = "rw";
            machine-inky = "ro";
          };
        };
      };
    };
  };

  shareNameCollisions = lib.intersectLists (lib.attrNames personal) (lib.attrNames household);
in
{
  imports = [ ./snapshots.nix ];

  options.custom.storage = {
    shares = lib.mkOption {
      description = "Shares served by this host, pairing the ZFS backing with the SMB export.";
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }:
          {
            options = {
              dataset = lib.mkOption {
                type = lib.types.str;
                description = "Dataset under the pool, without the pool name.";
              };
              root = lib.mkOption {
                type = lib.types.str;
                default = "/srv/storage/${name}";
              };
              backup = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "Whether to bind this share into the off-site backup. Opt-in, because the cost is per byte stored.";
              };
              snapshots = lib.mkOption {
                type = lib.types.bool;
                default = true;
              };
              childDatasets = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                description = "Child datasets under this share, snapshotted independently.";
              };
              smb = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = { };
                description = "Passed verbatim to `selfhost.storage.shares.smb.shares.<name>`, which is what types it. `path` and the child datasets come from the fields above.";
              };
            };
          }
        )
      );
    };
  };

  config = {
    custom.storage.shares =
      lib.mapAttrs (name: share: share // { dataset = "users/${name}"; }) personal
      // household;

    # Same rule as the SMB clients: a share named after someone in the registry is theirs.
    custom.shares = lib.mapAttrs (name: share: {
      inherit (share) root backup;
      personal = private.users ? ${name};
    }) cfg.shares;

    # Child datasets are already mounted, so they ride in with the plain directories: ownership only.
    selfhost.storage.shares.smb = {
      enable = true;
      openFirewall = true;
      shares = lib.mapAttrs (_: share:
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

    assertions = [
      {
        assertion = !(lib.any (lib.hasInfix "REPLACE_WITH") (lib.attrNames cfg.shares));
        message = "Storage share names still contain a placeholder; fill in the private host settings.";
      }
      {
        # The household set wins the merge above, so a collision would silently drop someone's dataset.
        assertion = shareNameCollisions == [ ];
        message = "A personal share name collides with a household share: ${toString shareNameCollisions}";
      }
    ];
  };
}
