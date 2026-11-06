{
  config,
  pkgs,
  lib,
  private,
  ...
}:
let
  cfg = config.my.storage;

  personal = private.settings.storage.personalShares;

  # disko declares the pool layout; a share adopts the dataset mounted at its root, plus its children.
  poolDatasets = config.disko.devices.zpool.tank.datasets;
  keyAt = root: lib.attrNames (lib.filterAttrs (_: d: d.mountpoint == root) poolDatasets);
  childKeys = key: lib.filter (n: lib.hasPrefix "${key}/" n) (lib.attrNames poolDatasets);

  household = {
    shared = {
      backup = true;
      owner = "bphenriques";
      gid = 989;
      smb.access = {
        groups.${private.groups.users} = "rw";
        serviceAccounts = {
          machine-compute = "rw";
          machine-laptop = "rw";
        };
      };
    };
    # The Immich library, which holds only app uploads; members reach their own photos as external libraries.
    immich-uploads = {
      root = "/srv/storage/immich/uploads";
      backup = true;
      owner = "bphenriques";
      gid = 985;
      # Machine-only: compute is the sole reader, and no human principal holds a grant here.
      smb.access.serviceAccounts.machine-compute = "rw";
    };
    immich-encoded-video = {
      root = "/srv/storage/immich/encoded-video";
      # Transcodes and motion-photo halves, regenerable from backed-up originals, so no snapshots or off-site copy.
      backup = false;
      snapshots = false;
      owner = "bphenriques";
      gid = 984;
      smb.access.serviceAccounts.machine-compute = "rw";
    };
    media = {
      backup = true;
      snapshots = false;
      owner = "bphenriques";
      gid = 990;
      directories = [
        "movies"
        "tv"
        "downloads"
        "downloads/incomplete"
      ]
      # The arrs health-check their category dir before any download would create it.
      ++ map (c: "downloads/${c}") (lib.attrValues config.fleet.downloadCategories);
      smb.access = {
        groups = {
          ${private.groups.admin} = "rw";
          ${private.groups.users} = "ro";
        };
        serviceAccounts = {
          machine-compute = "rw";
          machine-laptop = "rw";
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

  permissionRules = pkgs.writeText "storage-permissions.conf" (
    lib.concatLines (
      map (p: "d ${p} 0755 root root -") (lib.unique (map (s: dirOf s.root) (lib.attrValues cfg.shares)))
      ++ lib.concatLists (
        lib.mapAttrsToList (
          name: s:
          map (p: "d ${p} 2770 ${s.owner} storage-${name} -") (
            [ s.root ] ++ map (d: "${s.root}/${d}") (s.childDatasets ++ s.directories)
          )
        ) cfg.shares
      )
    )
  );
in
{
  options.my.storage = {
    shares = lib.mkOption {
      description = "Shares served by this host: ZFS dataset plus SMB export.";
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
                description = "Whether to include this share in the off-site backup.";
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
                description = "Child datasets, snapshotted independently.";
              };
              owner = lib.mkOption {
                type = lib.types.str;
                description = "Unix owner of the share root.";
              };
              gid = lib.mkOption {
                type = lib.types.int;
                description = "Pinned gid of the `storage-<name>` group that owns the share.";
              };
              directories = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                description = "Directories to create under the root.";
              };
              smb = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = { };
                description = "Passed to `selfhost.storage.shares.smb.shares.<name>`; `path` comes from `root`.";
              };
            };
          }
        )
      );
    };
  };

  config = {
    my.storage.shares = personal // household;

    # Same rule as the SMB clients.
    fleet.shares = lib.mapAttrs (name: share: {
      inherit (share) root backup;
      personal = private.users ? ${name};
    }) cfg.shares;

    users.groups = lib.mapAttrs' (name: share: lib.nameValuePair "storage-${name}" { inherit (share) gid; }) cfg.shares;

    # Not boot-time tmpfiles: the pool mounts `nofail`, and this must fail closed rather than write to root.
    systemd.services.storage-permissions = {
      description = "Prepare share ownership";
      wantedBy = [ "multi-user.target" ];
      unitConfig.RequiresMountsFor = lib.concatMap (s: [ s.root ] ++ map (c: "${s.root}/${c}") s.childDatasets) (
        lib.attrValues cfg.shares
      );
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.systemd}/bin/systemd-tmpfiles --create ${permissionRules}";
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
      };
    };

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
