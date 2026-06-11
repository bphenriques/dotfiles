{ config, pkgs, lib, ... }:
let
  serviceCfg = config.selfhost.services.radicale;

  usernames = builtins.attrNames (lib.filterAttrs (_: u: u.services.radicale.enable) config.selfhost.users);

  htpasswdFile = "/var/lib/radicale/users";
in
{
  imports = [ ./configure.nix ];

  options.selfhost.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.radicale.enable = lib.mkEnableOption "Radicale CalDAV/CardDAV access for this user";
    });
  };

  config = {
    selfhost = {
      services.radicale = {
        displayName = "Radicale";
        description = "CalDAV & CardDAV";
        port = 5232;
        subdomain = "radicale";
        access.allowedGroups = [ config.selfhost.groups.admin ];
        forwardAuth.enable = true;
        integrations.homepage.group = "Admin";
        healthcheck.path = "/.web/";
        healthcheck.probeModule = "http_any"; # Radicale requires htpasswd auth on all endpoints; 401 confirms service is up

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

      runtimeSecrets = lib.listToAttrs (map (uname: {
        name = "radicale-password-${uname}";
        value = {
          bytes = 24;
          regenerateIfMissing = false;
          restartUnits = [ "radicale-configure.service" ];
        };
      }) usernames);
    };

  # CalDAV/CardDAV endpoint without forwardAuth for regular sync. Uses Radicale's own htpasswd auth instead.
  # Includes .well-known redirects (RFC 6764) so DAVx5 and other clients can auto-discover the server.
  services.traefik.dynamicConfigOptions.http = {
    routers.radicale-dav = {
      rule = "Host(`dav.${config.selfhost.domain}`)";
      entryPoints = [ "websecure" ];
      service = "radicale-svc";
      middlewares = [ "radicale-wellknown" ];
    };
    middlewares.radicale-wellknown.redirectRegex = {
      regex = "^(https?://[^/]+)/\\.well-known/(caldav|carddav)/?$"; # Traefik matches full URL, not path
      replacement = "\${1}/";
      permanent = false;
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
  };
}
