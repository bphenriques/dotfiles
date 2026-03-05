{ lib, config, pkgs, utils, ... }:
let
  inherit (lib) mkOption types;

  cfg = config.custom.homelab.cifs;

  # Convert a path to a systemd unit name (e.g., /mnt/homelab-bphenriques -> mnt-homelab\x2dbphenriques)
  pathToUnitName = path: utils.escapeSystemdPath (lib.removePrefix "/" path);

  cifsCfg = types.submodule ({ name, config, ... }: {
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
        description = "GID for the mount group (required for CIFS mount options)";
      };
      systemd.automountUnit = mkOption {
        type = types.str;
        default = "${pathToUnitName config.localMount}.automount";
        readOnly = true;
        description = "Systemd automount unit name for this mount";
      };
      systemd.dependentServices = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          Systemd services that depend on this mount being available.

          For each service listed, the module automatically adds:
            - requires = [ automountUnit ]
            - after = [ automountUnit ]

          The automount unit is always active; actual mount happens on first
          access to the mount point. This avoids boot failures when the network
          isn't fully ready yet while still ensuring proper service ordering.

          Group membership for mount access must still be configured explicitly
          via users.users.<name>.extraGroups.

          Example:
            custom.homelab.cifs.mounts.media.systemd.dependentServices = [ "jellyfin" "kavita" ];
        '';
      };
    };
  });
in {
  options.custom.homelab.cifs = {
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
      default = config.sops.templates."homelab-samba-credentials".path;
      description = "Path to the CIFS credentials file";
      readOnly = true;
    };
    
    mounts = mkOption {
      type = types.attrsOf cifsCfg;
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
    
    sops = {
      secrets."homelab/samba/username" = { };
      secrets."homelab/samba/password" = { };
      templates."homelab-samba-credentials" = {
        content = ''
          username=${config.sops.placeholder."homelab/samba/username"}
          password=${config.sops.placeholder."homelab/samba/password"}
        '';
      };
    };

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
          # Credentials
          "credentials=${cfg.credentialsPath}"
          # Automount: don't mount at boot, trigger on first access.
          # This avoids boot failures when network isn't fully ready yet.
          "_netdev"
          "x-systemd.automount"
          "noauto"
          "x-systemd.idle-timeout=15"
          "x-systemd.device-timeout=5s"
          "x-systemd.mount-timeout=5s"
        ];
      }
    ) cfg.mounts;

    # Auto-wire systemd dependencies for services that depend on mounts
    systemd.services = lib.mkMerge (
      lib.mapAttrsToList (_: mountCfg:
        lib.listToAttrs (map (svcName: {
          name = svcName;
          value = {
            requires = [ mountCfg.systemd.automountUnit ];
            after = [ mountCfg.systemd.automountUnit ];

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
