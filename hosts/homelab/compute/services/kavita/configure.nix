{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  oidcClient = oidcCfg.clients.kavita;
  kavitaCfg = config.services.kavita;

  credentialsDir = "${kavitaCfg.dataDir}/credentials";

  kavitaConfigJson = builtins.toJSON {
    kavitaUrl = serviceCfg.internalUrl;
    adminPasswordFile = "${credentialsDir}/admin-password";

    oidc = {
      authority = oidcCfg.provider.url;
      buttonText = "Login with ${oidcCfg.provider.displayName}";
      provisionAccounts = true;
      syncUserSettings = false;
      rolesClaim = "groups";
      rolesPrefix = "kavita-";
      defaultRoles = [ "Login" ];
    };

    libraries = [
      { name = "Books"; type = 2; folders = [ "/mnt/media/books" ]; }
      { name = "Comics"; type = 1; folders = [ "/mnt/media/comics" ]; }
    ];
  };
in
{
  systemd.services.kavita-configure = {
    description = "Configure Kavita OIDC and libraries";
    wantedBy = [ "kavita.service" ];
    after = [ "kavita.service" ];
    requires = [ "kavita.service" ];
    restartTriggers = [ kavitaConfigJson ./kavita-configure.nu ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = kavitaCfg.user;
      Group = kavitaCfg.user;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
      LoadCredential = oidcClient.systemd.loadCredentials;
      SupplementaryGroups = oidcClient.systemd.supplementaryGroups;
    };
    environment = {
      KAVITA_URL = serviceCfg.internalUrl;
      KAVITA_DATA_DIR = kavitaCfg.dataDir;
      KAVITA_CONFIG_FILE = pkgs.writeText "kavita-config.json" kavitaConfigJson;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "kavita-configure" ./kavita-configure.nu}'';
  };
}
