{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.romm;
  dataDir = "/var/lib/romm";
  credentialsDir = "${dataDir}/credentials";

  adminConfig = {
    username = "admin";
    email = "admin@romm.local";
  };
in
{
  systemd.tmpfiles.rules = [
    "d ${credentialsDir} 0700 root root -"
  ];

  systemd.services.romm-configure = {
    description = "Configure RomM admin user";
    wantedBy = [ "multi-user.target" ];
    after = [ "podman-romm.service" ];
    requires = [ "podman-romm.service" ];
    partOf = [ "podman-romm.service" ];
    restartTriggers = [ (builtins.toJSON adminConfig) ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      ROMM_URL = serviceCfg.internalUrl;
      ROMM_ADMIN_USERNAME = adminConfig.username;
      ROMM_ADMIN_EMAIL = adminConfig.email;
      ROMM_ADMIN_PASSWORD_FILE = "${credentialsDir}/admin-password";
    };
    path = [ pkgs.nushell pkgs.curl ];
    script = ''nu ${self.lib.builders.writeNushellScript "romm-configure" ./romm-configure.nu}'';
  };
}
