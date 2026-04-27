{ lib, config, pkgs, ... }:
let
  inherit (lib) mkOption types;

  cfg = config.custom.homelab.smb;
  homelabCfg = config.custom.homelab;

  # Resolve which systemd units a service needs for its storage mounts.
  # Priority: explicit storage.systemdServices > OCI container auto-detect > service name.
  ociContainers = config.virtualisation.oci-containers.containers or {};
  ociBackend = config.virtualisation.oci-containers.backend or "podman";

  resolveServiceUnits = svc:
    if svc.storage.systemdServices != [ ]
    then svc.storage.systemdServices
    else if ociContainers ? ${svc.name}
    then [ "${ociBackend}-${svc.name}" ]
    else [ svc.name ];

  # Services and tasks that declare storage mounts
  servicesWithStorage = lib.filter (svc: svc.storage.smb != [ ]) (lib.attrValues homelabCfg.services);
  tasksWithStorage = lib.filter (task: task.storage.smb != [ ]) (lib.attrValues homelabCfg.tasks);

  # Build mount→units mapping from service registry
  serviceMountDeps = lib.foldl' (acc: svc:
    let units = resolveServiceUnits svc;
    in lib.foldl' (acc2: mountName:
      acc2 // { ${mountName} = (acc2.${mountName} or [ ]) ++ units; }
    ) acc svc.storage.smb
  ) { } servicesWithStorage;

  # Build mount→units mapping from task registry
  taskMountDeps = lib.foldl' (acc: task:
    lib.foldl' (acc2: mountName:
      acc2 // { ${mountName} = (acc2.${mountName} or [ ]) ++ task.systemdServices; }
    ) acc task.storage.smb
  ) { } tasksWithStorage;

  # Merge registry-derived deps with explicit dependentServices per mount
  allDependentUnits = mountName: mountCfg:
    lib.unique (
      mountCfg.systemd.dependentServices
      ++ (serviceMountDeps.${mountName} or [ ])
      ++ (taskMountDeps.${mountName} or [ ])
    );

  hasDepServices = mountName: mountCfg: allDependentUnits mountName mountCfg != [];

  smbMountCfg = types.submodule ({ name, config, ... }: {
    options = {
      localMount = mkOption {
        type = types.str;
        default = "/mnt/homelab-${name}";
        description = "Local mount point for the share";
      };
      remote = mkOption {
        type = types.str;
        default = name;
        readOnly = true;
        description = "Remote folder name on the homelab server";
      };
      group = mkOption {
        type = types.str;
        default = "homelab-${name}";
        description = "Name of the group with access to the mount";
      };
      gid = mkOption {
        type = types.int;
        description = "GID for the mount group (required for SMB mount options)";
      };
      systemd.dependentServices = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          Systemd services that depend on this mount being available.
          Use for non-registry units or dynamic cases (e.g., Immich per-user mounts).
          Registry services should use storage.smb instead.
        '';
      };
    };
  });
in {
  options.custom.homelab.smb = {
    enable = lib.mkEnableOption "Home-server storage";

    hostname = mkOption {
      type = types.str;
      description = ''
        IP address or hostname of the homelab server.

        Prefer using an IP address or a hostname defined in /etc/hosts
        to ensure reliable resolution at boot time (before mDNS/Avahi is ready).
      '';
    };
    
    credentialsPath = mkOption {
      type = types.str;
      description = "Path to the SMB credentials file (must be provided by the host, e.g. via sops-nix)";
    };
    
    mounts = mkOption {
      type = types.attrsOf smbMountCfg;
      default = {};
      description = "Attributes where the key is the remote root folder to configure";
      example = lib.literalExpression ''
        {
          bphenriques = { };
          media = { };
        }
      '';
    };
  };

  config = lib.mkIf cfg.enable {

    assertions = let
       # Collision detection for GIDs
       allGids = lib.mapAttrsToList (_: m: m.gid) cfg.mounts;
       dupGids = lib.filter (gid: lib.count (g: g == gid) allGids > 1) (lib.unique allGids);

       # Verify resolved systemd units exist
       allResolvedUnits = lib.concatMap resolveServiceUnits servicesWithStorage
         ++ lib.concatMap (task: task.systemdServices) tasksWithStorage;
       missingUnits = lib.filter (unit: !(config.systemd.services ? ${unit})) allResolvedUnits;

       # Tasks with storage mounts must declare systemdServices
       tasksMissingUnits = lib.filter (task: task.storage.smb != [ ] && task.systemdServices == [ ]) (lib.attrValues homelabCfg.tasks);
     in [
      {
        assertion = dupGids == [];
        message = "Homelab mounts have duplicate gids: ${toString dupGids}";
      }
      {
        assertion = missingUnits == [];
        message = "Storage mount wiring references unknown systemd units: ${lib.concatStringsSep ", " missingUnits}. Set storage.systemdServices explicitly if the unit name differs from the service name.";
      }
      {
        assertion = tasksMissingUnits == [];
        message = "Tasks with storage.smb must declare systemdServices: ${lib.concatMapStringsSep ", " (t: t.name) tasksMissingUnits}";
      }
    ];

    environment.systemPackages = [ pkgs.cifs-utils ];
    
    users.groups = lib.mapAttrs' (_name: mountCfg: lib.nameValuePair mountCfg.group { inherit (mountCfg) gid; } ) cfg.mounts;

    fileSystems = lib.mapAttrs' (name: mountCfg:
      lib.nameValuePair mountCfg.localMount {
        device = "//${cfg.hostname}/${mountCfg.remote}";
        fsType = "cifs";
        options = [
          # Permissions
          "uid=0"
          "gid=${toString mountCfg.gid}"
          "file_mode=0660"
          "dir_mode=0770"

          # Security: nosuid/nodev/noexec, SMB3 for encryption
          "nosuid"
          "nodev"
          "noexec"
          "vers=3.0"

          "credentials=${cfg.credentialsPath}"

          "_netdev"
        ]
        # Mounts with dependentServices use boot-time mounting: RequiresMountsFor (used to wire
        # service dependencies) conflicts with x-systemd.automount because it forces the mount
        # immediately at service start, defeating lazy mounting and hitting the network race window.
        # Instead, we mount at boot with nofail (non-blocking) and a generous timeout, relying on
        # the service retry logic below as a safety net.
        #
        # Mounts without dependentServices use automount (lazy mount on first access), which avoids
        # boot-time races entirely — particularly important on WiFi where routing may not be ready
        # when network-online.target is reached.
        ++ (if hasDepServices name mountCfg then [
          "nofail"
          "x-systemd.mount-timeout=30s"
        ] else [
          "noauto"
          "x-systemd.automount"
          "x-systemd.mount-timeout=30s"
        ]);
      }
    ) cfg.mounts;

    # Auto-wire systemd dependencies for services that depend on mounts
    systemd.services = lib.mkMerge (
      lib.mapAttrsToList (name: mountCfg:
        lib.listToAttrs (map (svcName: {
          name = svcName;
          value = {
            unitConfig.RequiresMountsFor = [ mountCfg.localMount ];

            # Retry with delays if mount isn't ready yet (network filesystem race at boot)
            serviceConfig = {
              Restart = lib.mkDefault "on-failure";
              RestartSec = lib.mkDefault "10s";
            };
            startLimitIntervalSec = lib.mkDefault 180;  # 3 minutes
            startLimitBurst = lib.mkDefault 10;
          };
        }) (allDependentUnits name mountCfg))
      ) cfg.mounts
    );
  };
}
