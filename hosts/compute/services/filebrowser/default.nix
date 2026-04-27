{ config, pkgs, lib, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.filebrowser;
  homelabMounts = cfg.smb.mounts;

  filebrowserRoot = "/var/lib/filebrowser/root";
in
{
  imports = [ ./configure.nix ];

  options.custom.homelab.users = lib.mkOption {
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
    custom.homelab.services.filebrowser = {
      displayName = "File Browser";
      metadata.description = "File Browser";
      metadata.version = pkgs.filebrowser.version;
      metadata.homepage = pkgs.filebrowser.meta.homepage;
      metadata.category = "Home";
      port = 8085;
      subdomain = "files";
      access.allowedGroups = with cfg.groups; [ guests users admin ];
      forwardAuth.enable = true;
      integrations.homepage.enable = true;

      # Upload size limit (4GB). A protection on top of Synology quota
      traefik.middlewares.filebrowser-buffering.buffering.maxRequestBodyBytes = 4294967296;
      storage.smb = [ "shared" "bphenriques" ];
    };

    custom.homelab.external.shared-files = {
      displayName = "Shared Files";
      description = "Shared Files";
      category = "General";
      url = "https://shared.${cfg.domain}";
      icon = "filebrowser.svg";
    };

    # Guest access: separate subdomain with BasicAuth (no Pocket-ID required).
    # BasicAuth's headerField sets Remote-User=guest, which FileBrowser uses for proxy auth.
    # Guest user is pre-created by filebrowser-configure with read-only permissions and /shared scope.
    sops.secrets."filebrowser/guest-htpasswd" = { owner = "traefik"; };
    services.traefik.dynamicConfigOptions.http = {
      routers.filebrowser-guest = {
        rule = "Host(`shared.${cfg.domain}`)";
        entryPoints = [ "websecure" ];
        service = "filebrowser-svc";
        middlewares = [ "filebrowser-guest-auth" "filebrowser-buffering" ];
      };
      middlewares.filebrowser-guest-auth.basicAuth = {
        usersFile = config.sops.secrets."filebrowser/guest-htpasswd".path;
        headerField = "Remote-User";
        removeHeader = true;
      };
    };

    # Proxy auth via Traefik forwardAuth (Remote-User). Local bypass on 127.0.0.1 acceptable.
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
      homelabMounts.shared.group
      homelabMounts.bphenriques.group
    ];

    systemd.services.filebrowser = {
      after = [ "filebrowser-configure.service" ];
      requires = [ "filebrowser-configure.service" ];
      serviceConfig = {
        # BindPaths avoids global bind mounts that race with CIFS automount
        BindPaths = [
          "${homelabMounts.shared.localMount}:${filebrowserRoot}/shared"
          "${homelabMounts.bphenriques.localMount}:${filebrowserRoot}/bphenriques"
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
