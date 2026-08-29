{
  config,
  lib,
  private,
  ...
}:
let
  cfg = config.custom.storage;

  personal = private.settings.storage.personalShares;

  # disko declares the pool layout; a share adopts the dataset mounted at its root, plus its children.
  poolDatasets = config.disko.devices.zpool.tank.datasets;
  keyAt = root: lib.attrNames (lib.filterAttrs (_: d: d.mountpoint == root) poolDatasets);
  childKeys = key: lib.filter (n: lib.hasPrefix "${key}/" n) (lib.attrNames poolDatasets);

  household = {
    shared = {
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
      backup = true;
      snapshots = false;
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

  coveredKeys =
    let
      matched = lib.concatMap (s: keyAt s.root) (lib.attrValues cfg.shares);
    in
    matched ++ lib.concatMap childKeys matched;

  strayDatasets = lib.subtractLists coveredKeys (
    lib.attrNames (lib.filterAttrs (_: d: d.mountpoint != null) poolDatasets)
  );

  orphanShares = lib.attrNames (lib.filterAttrs (_: s: s.dataset == "") cfg.shares);

  shareNameCollisions = lib.intersectLists (lib.attrNames personal) (lib.attrNames household);
in
{
  options.custom.storage = {
    shares = lib.mkOption {
      description = "Shares served by this host, pairing the ZFS backing with the SMB export.";
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, config, ... }:
          let
            key = lib.head (keyAt config.root ++ [ "" ]);
          in
          {
            options = {
              dataset = lib.mkOption {
                type = lib.types.str;
                readOnly = true;
                default = if key == "" then "" else poolDatasets.${key}._name;
                defaultText = lib.literalMD "the disko dataset mounted at `root`";
                description = "Fully qualified dataset backing this share, read back from disko.";
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
                readOnly = true;
                default = map (lib.removePrefix "${key}/") (childKeys key);
                defaultText = lib.literalMD "disko datasets nested under `dataset`";
                description = "Child datasets under this share, snapshotted independently and ownership-fixed with it.";
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
    custom.storage.shares = personal // household;

    # Same rule as the SMB clients.
    custom.shares = lib.mapAttrs (name: share: {
      inherit (share) root backup;
      personal = private.users ? ${name};
    }) cfg.shares;

    warnings = lib.optional (strayDatasets != [ ])
      "Datasets mounted here but reached by no share: ${toString strayDatasets}. They are not snapshotted, exported or backed up. Declare a share, or `zfs destroy` them if a removal is unfinished.";

    assertions = [
      {
        # Without a dataset the share would export whatever sits on the root filesystem at that path.
        assertion = orphanShares == [ ];
        message = "Shares whose `root` is not a disko dataset mountpoint: ${toString orphanShares}. Declare the dataset in hosts/storage/disko/pool.nix.";
      }
      {
        # The household set wins the merge above, so a collision would silently drop someone's dataset.
        assertion = shareNameCollisions == [ ];
        message = "A personal share name collides with a household share: ${toString shareNameCollisions}";
      }
    ];
  };
}
