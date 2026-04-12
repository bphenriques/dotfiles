{ lib, config, pkgs, ... }:
let
  inherit (lib) mkOption types;

  cfg = config.custom.homelab.smb;

  hasDepServices = mountCfg: mountCfg.systemd.dependentServices != [];

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

          For each service listed, the module automatically adds:
            - unitConfig.RequiresMountsFor = [ localMount ]
            - Restart/retry configuration for network filesystem races

          Group membership for mount access must still be configured explicitly
          via users.users.<name>.extraGroups.

          Example:
            custom.homelab.smb.mounts.media.systemd.dependentServices = [ "jellyfin" "kavita" ];
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
     in [{
      assertion = dupGids == [];
      message = "Homelab mounts have duplicate gids: ${toString dupGids}";
    }];

    environment.systemPackages = [ pkgs.cifs-utils ];
    
    users.groups = lib.mapAttrs' (name: mountCfg: lib.nameValuePair mountCfg.group { gid = mountCfg.gid; } ) cfg.mounts;

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
        ++ (if hasDepServices mountCfg then [
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
      lib.mapAttrsToList (_: mountCfg:
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
        }) mountCfg.systemd.dependentServices)
      ) cfg.mounts
    );
  };
}
