{ config, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.gitea;
  oidcCfg = config.custom.homelab.oidc;
in
{
  custom.homelab.services.gitea.secrets = {
    files.admin-password = { rotatable = false; };
    systemd.dependentServices = [ "gitea" "gitea-configure" ];
  };

  systemd.services.gitea-configure = {
    description = "Gitea setup";
    wantedBy = [ "gitea.service" ];
    after = [ "gitea.service" ];
    requires = [ "gitea.service" ];
    partOf = [ "gitea.service" ];
    restartTriggers = [ ./gitea-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
      User = config.services.gitea.user;
      Group = config.services.gitea.group;
      WorkingDirectory = config.services.gitea.stateDir;
      SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
    };
    environment = {
      GITEA_URL = serviceCfg.url;
      GITEA_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
      GITEA_CONFIG = "${config.services.gitea.stateDir}/custom/conf/app.ini";
      OIDC_PROVIDER_NAME = oidcCfg.provider.internalName;
      OIDC_DISPLAY_NAME = oidcCfg.provider.displayName;
      OIDC_DISCOVERY_URL = "${oidcCfg.provider.issuerUrl}/.well-known/openid-configuration";
      OIDC_CLIENT_ID_FILE = serviceCfg.oidc.id.file;
      OIDC_CLIENT_SECRET_FILE = serviceCfg.oidc.secret.file;
    };
    path = [ config.services.gitea.package pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "gitea-configure" ./gitea-configure.nu}'';
  };
}
