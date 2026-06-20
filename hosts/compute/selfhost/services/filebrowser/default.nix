{ config, pkgs, lib, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.filebrowser;
  selfhostMounts = cfg.storage.smb.mounts;

  filebrowserRoot = "/var/lib/filebrowser/root";
in
{
  imports = [ ./configure.nix ];

  options.selfhost.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.filebrowser = {
        enable = lib.mkEnableOption "FileBrowser account for this user";
        scope = lib.mkOption {
          type = lib.types.str;
          default = "/shared";
          description = "Directory scope relative to FileBrowser root";
        };
        admin = lib.mkEnableOption "admin access";
        permissions = lib.mkOption {
          type = lib.types.nullOr (lib.types.attrsOf lib.types.bool);
          default = null;
          description = "Per-user permission overrides (null = inherit defaults)";
        };
      };
    });
  };

  config = {
    selfhost.services.filebrowser = {
      displayName = "File Browser";
      description = "File Browser";
      port = 8085;
      subdomain = "files";
      access.allowedGroups = with cfg.groups; [ users admin ];
      forwardAuth.enable = true;

      # Upload size limit (4GB). A protection on top of Synology quota
      traefik.middlewares.filebrowser-buffering.buffering.maxRequestBodyBytes = 4294967296;
      storage.smb = [ "shared" "bphenriques" ];
    };

    # Proxy auth via Traefik forwardAuth (Remote-User). Local bypass on 127.0.0.1 acceptable.
    # Public/anonymous sharing is handled by share-vm, not a guest path here.
    services.filebrowser = {
      enable = true;
      settings = {
        address = "127.0.0.1";
        inherit (serviceCfg) port;
        root = filebrowserRoot;
        database = "/var/lib/filebrowser/filebrowser.db";
      };
    };

    users.users.filebrowser.extraGroups = [
      selfhostMounts.shared.group
      selfhostMounts.bphenriques.group
    ];

    systemd.services.filebrowser = {
      after = [ "filebrowser-configure.service" ];
      requires = [ "filebrowser-configure.service" ];
      serviceConfig = {
        # BindPaths avoids global bind mounts that race with CIFS automount
        BindPaths = [
          "${selfhostMounts.shared.localMount}:${filebrowserRoot}/shared"
          "${selfhostMounts.bphenriques.localMount}:${filebrowserRoot}/bphenriques"
        ];

        Restart = lib.mkForce "on-failure";
        RestartSec = "5s";

        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectClock = true;
        ProtectKernelLogs = true;
      };
    };
  };
}
