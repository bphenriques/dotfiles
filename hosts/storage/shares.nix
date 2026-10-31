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
    # Immich's media location lives on compute; the two folders that actually grow are mounted from
    # here. `immich-uploads` is Immich's `library/`, which holds only what people upload through the
    # app: household members reach their own photos as external libraries from their personal share.
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
      # Transcodes, plus the motion-photo halves Immich extracts here. All of it regenerates from
      # originals that are themselves backed up, so neither snapshotted nor sent off-site; the build
      # warning naming this share is the intended opt-out record.
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
                description = "Child datasets under this share, snapshotted independently and owned like it.";
              };
              owner = lib.mkOption {
                type = lib.types.str;
                description = "Unix owner of the share root.";
              };
              gid = lib.mkOption {
                type = lib.types.int;
                description = "Pinned gid of `storage-<name>`, the group owning everything in the share; the data outlives the root that would record an allocation.";
              };
              directories = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = [ ];
                description = "Plain directories under the root, created with the share's ownership.";
              };
              smb = lib.mkOption {
                type = lib.types.attrsOf lib.types.anything;
                default = { };
                description = "Passed verbatim to `selfhost.storage.shares.smb.shares.<name>`, which is what types it. `path` comes from `root`.";
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

    # Not boot-time tmpfiles: the pool mounts with `nofail`, and this must fail closed rather than create
    # directories on the root filesystem. Parents stay traversable, since smbd drops to the connecting user.
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
