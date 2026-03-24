{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.radicale;

  usernames = builtins.attrNames (lib.filterAttrs (_: u: u.services.radicale.enable) config.custom.homelab.users);

  htpasswdFile = "/var/lib/radicale/users";
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.radicale = {
    displayName = "Radicale";
    metadata.description = "CalDAV & CardDAV";
    metadata.version = pkgs.radicale.version;
    metadata.homepage = pkgs.radicale.meta.homepage;
    metadata.category = "General";
    port = 5232;
    subdomain = "radicale";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.homepage.tab = "Admin";
    healthcheck.path = "/.web/";
    healthcheck.probeModule = "http_any"; # Radicale requires htpasswd auth on all endpoints; 401 confirms service is up

    secrets = {
      files = lib.listToAttrs (map (uname: {
        name = "password-${uname}";
        value = { rotatable = false; bytes = 24; };
      }) usernames);
      systemd.dependentServices = [ "radicale" "radicale-configure" ];
    };

    backup = {
      package = pkgs.writeShellApplication {
        name = "backup-radicale";
        text = ''
          export RADICALE_DATA="/var/lib/radicale/collections"

          # shellcheck disable=SC1091
          source ${./backup.sh}
        '';
      };
      after = [ "radicale.service" ];
    };
  };

  # CalDAV/CardDAV endpoint without forwardAuth for regular sync. Uses Radicale's own htpasswd auth instead.
  services.traefik.dynamicConfigOptions.http = {
    routers.radicale-dav = {
      rule = "Host(`dav.${config.custom.homelab.domain}`)";
      entryPoints = [ "websecure" ];
      service = "radicale-svc";
    };
  };

  services.radicale = {
    enable = true;
    package = pkgs.radicale;
    settings = {
      auth = {
        type = "htpasswd";
        htpasswd_filename = htpasswdFile;
        htpasswd_encryption = "bcrypt";
      };
      server.hosts = [ "127.0.0.1:${toString serviceCfg.port}" ];
      storage.filesystem_folder = "/var/lib/radicale/collections";
    };
  };
}
