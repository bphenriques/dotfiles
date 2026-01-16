{ lib, config, pkgs, ... }:
let
  inherit (lib) mkOption types mkIf mkEnableOption;

  cfg = config.custom.fileSystems.homelab;

  cifsCfg = types.submodule ({ name, ... }: {
    options = {
      localMount = mkOption {
        type = types.str;
        default = "/mnt/homelab-${name}";
        description = "Local mount point for the share";
      };
      remote = mkOption {
        type = types.str;
        default = name;
        description = "Remote folder name on the homelab server";
      };
    };
  });

  mkCifsFs = name: mountCfg: {
    fileSystems."${mountCfg.localMount}" = {
      device = "//${cfg.hostname}/${mountCfg.remote}";
      fsType = "cifs";
      options = [
        # Use homelab group for all mounts
        "uid=0"
        "gid=${toString config.users.groups.homelab.gid}"
        "file_mode=0660"
        "dir_mode=0770"
        # Credentials
        "credentials=${config.sops.templates."homelab-samba-credentials".path}"
        # Network split protection
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=15"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
      ];
    };

    systemd.tmpfiles.rules = [
      "d ${mountCfg.localMount} 0770 root homelab -"
    ];
  };

  mountConfigs = lib.attrsets.mapAttrsToList mkCifsFs cfg.mounts;
in {
  options.custom.fileSystems.homelab = {
    enable = mkEnableOption "Home-server storage";
    
    hostname = mkOption {
      type = types.str;
      default = "bruno-home-nas";
      description = "Hostname or IP of the homelab server";
    };
    
    mounts = mkOption {
      type = types.attrsOf cifsCfg;
      description = "Attributes where the key is the remote root folder to configure";
      default = {
        bphenriques = { };
        media = { };
      };
      example = lib.literalExpression ''
        {
          bphenriques = { };
          media = { };
        }
      '';
    };
  };

  config = mkIf cfg.enable (lib.mkMerge ([
    {
      users.groups.homelab = { }; # gid assigned automatically

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
    }
  ] ++ mountConfigs));
}