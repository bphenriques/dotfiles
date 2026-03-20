{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.radicale;

  usernames = builtins.attrNames (lib.filterAttrs (_: u: u.services.radicale.enable) config.custom.homelab.users);

  htpasswdFile = "/var/lib/radicale/users";
  configFile = pkgs.writeText "radicale-configure.json" (builtins.toJSON {
    inherit htpasswdFile;
    users = lib.listToAttrs (map (uname: {
      name = uname;
      value = { passwordFile = serviceCfg.secrets.files."password-${uname}".path; };
    }) usernames);
  });
in
{
  custom.homelab.services.radicale = {
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
    integrations.catalogue.displayName = "Radicale";
    healthcheck.path = "/.web/";

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

  systemd.services.radicale-configure = {
    description = "Generate Radicale htpasswd from homelab users";
    requiredBy = [ "radicale.service" ];
    before = [ "radicale.service" ];
    restartTriggers = [ configFile ./radicale-configure.nu ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      UMask = "0027";
    };
    environment.RADICALE_PROVISION_FILE = configFile;
    path = [ pkgs.apacheHttpd pkgs.nushell pkgs.coreutils ];
    script = ''nu ${self.lib.builders.writeNushellScript "radicale-configure" ./radicale-configure.nu}'';
  };
}
