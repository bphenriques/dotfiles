{ config, pkgs, lib, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.filebrowser;
  homelabMounts = cfg.smb.mounts;

  filebrowserRoot = "/var/lib/filebrowser/root";
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.filebrowser = {
    description = "File Browser";
    version = pkgs.filebrowser.version;
    homepage = pkgs.filebrowser.meta.homepage;
    category = "General";
    port = 8085;
    subdomain = "files";
    forwardAuth = {
      enable = true;
      groups = with cfg.groups; [ users admin ];
    };
    integrations.homepage.enable = true;

    # Upload size limit (4GB). A protection on top of Synology quota
    traefik.middlewares.filebrowser-buffering.buffering.maxRequestBodyBytes = 4294967296;
  };

  custom.homelab.smb.mounts = {
    shared.systemd.dependentServices = [ "filebrowser" ];
    bphenriques.systemd.dependentServices = [ "filebrowser" ];
  };

  # FileBrowser with proxy auth: Traefik forwardAuth handles authentication and sends `Remote-User` header.
  # Security: boundAny local process can spoof `Remote-User` and gain access. Acceptable.
  # Traefik does not support unix socket backends, so this is the best available option.
  services.filebrowser = {
    enable = true;
    settings = {
      address = "127.0.0.1";
      port = serviceCfg.port;
      root = filebrowserRoot;
      database = "/var/lib/filebrowser/filebrowser.db";
    };
  };

  # Additions on top of the NixOS module: SMB mount access, bind mounts, explicit ordering, extra hardening
  users.users.filebrowser.extraGroups = [
    homelabMounts.shared.group
    homelabMounts.bphenriques.group
  ];

  systemd.services.filebrowser = {
    after = [ "filebrowser-configure.service" ];
    requires = [ "filebrowser-configure.service" ];
    serviceConfig = {
      # Bind SMB mounts into FileBrowser's view — avoids global bind mounts that can race with CIFS automount
      BindPaths = [
        "${homelabMounts.shared.localMount}:${filebrowserRoot}/shared"
        "${homelabMounts.bphenriques.localMount}:${filebrowserRoot}/bphenriques"
      ];

      Restart = lib.mkForce "on-failure";
      RestartSec = "5s";

      # Extra hardening beyond what the NixOS module provides
      ProtectSystem = "strict";
      ProtectHome = true;
      ProtectClock = true;
      ProtectKernelLogs = true;
    };
  };
}
