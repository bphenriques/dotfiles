{ config, pkgs, lib, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.filebrowser;
  selfhostMounts = cfg.storage.smb.mounts;

  filebrowserRoot = "/var/lib/filebrowser/root";
in
{
  config = {
    selfhost = {
      apps.filebrowser.enable = true;
      services.filebrowser = {
        access.allowedGroups = with cfg.groups; [ users admin ];
        traefik.middlewares.filebrowser-buffering.buffering.maxRequestBodyBytes = 4294967296; # 4GB upload cap
      };
    };

    services.filebrowser = {
      enable = true;
      settings = {
        address = "127.0.0.1";
        inherit (serviceCfg) port;
        root = filebrowserRoot;
        branding = {
          disableExternal = true;
          disableUsedPercentage = true;
        };
        viewMode = "mosaic";
        singleClick = true;
        hideDotfiles = true;
        sorting = { by = "modified"; asc = false; };
      };
    };
    users.users.filebrowser.extraGroups = map (m: selfhostMounts.${m}.group) serviceCfg.storage.smb;

    # Default empty folders
    services.filebrowser-multiuser.unlistedScope = "/empty"; # a group member not listed lands here
    systemd.tmpfiles.rules = [
      "d ${filebrowserRoot}/empty 0700 ${config.services.filebrowser.user} ${config.services.filebrowser.group} -"
    ];

    systemd.services.filebrowser = {
      after = [ "filebrowser-configure.service" ];
      requires = [ "filebrowser-configure.service" ];
      serviceConfig = {
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
