{ lib, config, pkgs, utils, ... }:
let
  inherit (lib) mkOption types;

  cfg = config.custom.fileSystems.homelab;

  # Generate a deterministic GID from the mount name (range 5000 to 5999). Unlikely to clash but may happen.
  baseGid = 5000;
  nameToGid = name:
    let charSum = lib.foldl (acc: c: acc + lib.strings.charToInt c) 0 (lib.stringToCharacters name);
    in baseGid + (lib.mod charSum 1000);

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
        default = nameToGid name;
        description = "GID for the mount group (auto-generated from name)";
      };
      automountUnit = mkOption {
        type = types.str;
        default = "${pathToUnitName config.localMount}.automount";
        readOnly = true;
        description = "Systemd automount unit name for this mount";
      };
    };
  });
in {
  options.custom.fileSystems.homelab = {
    enable = lib.mkEnableOption "Home-server storage";
    
    hostname = mkOption {
      type = types.str;
      default = "bruno-home-nas";
      description = "Hostname or IP of the homelab server";
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
    environment.systemPackages = [ pkgs.cifs-utils ];
    
    sops = {
      secrets.homelab_samba_username = { };
      secrets.homelab_samba_password = { };
      templates."homelab-samba-credentials" = {
        content = ''
          username=${config.sops.placeholder.homelab_samba_username}
          password=${config.sops.placeholder.homelab_samba_password}
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
          # Network Split
          "x-systemd.automount"
          "noauto"
          "x-systemd.idle-timeout=15"
          "x-systemd.device-timeout=5s"
          "x-systemd.mount-timeout=5s"
        ];
      }
    ) cfg.mounts;

    # FIXME: Do I realy need this?
    #systemd.tmpfiles.rules = lib.mapAttrsToList (name: mountCfg:
    #  "d ${mountCfg.localMount} 0770 root ${mountCfg.group} -"
    #) cfg.mounts;
  };
}
