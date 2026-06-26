{ config, pkgs, lib, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.filebrowser;
  selfhostMounts = cfg.storage.smb.mounts;

  filebrowserRoot = "/var/lib/filebrowser/root";
in
{
  config = {
    # Internal, forwardAuth'd instance; public/anonymous sharing is share-vm's job.
    # displayName/description/port come from the app defaults; storage.smb is derived from per-user grants.
    selfhost.services.filebrowser = {
      subdomain = "files";
      access.allowedGroups = with cfg.groups; [ users admin ];
      traefik.middlewares.filebrowser-buffering.buffering.maxRequestBodyBytes = 4294967296; # 4GB upload cap
    };

    services.filebrowser = {
      enable = true;
      settings = {
        address = "127.0.0.1"; # forwardAuth gates ingress; localhost is the only pre-auth surface
        inherit (serviceCfg) port;
        root = filebrowserRoot;
        database = "/var/lib/filebrowser/filebrowser.db";
        branding = { disableExternal = true; disableUsedPercentage = true; };
        viewMode = "mosaic";
        singleClick = true;
        hideDotfiles = true;
        sorting = { by = "modified"; asc = false; };
      };
    };

    selfhost.apps.filebrowser.enable = true; # enableSelfhostIntegration defaults on (derive users + binds from selfhost.users)
    services.filebrowser-multiuser.unlistedScope = "/empty"; # a group member not listed lands here — no ambient access

    # The empty default scope; read access follows the grant-derived mount inventory.
    systemd.tmpfiles.rules = [ "d ${filebrowserRoot}/empty 0700 ${config.services.filebrowser.user} ${config.services.filebrowser.group} -" ];
    users.users.filebrowser.extraGroups = map (m: selfhostMounts.${m}.group) serviceCfg.storage.smb;

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
