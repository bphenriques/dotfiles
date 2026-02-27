{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  kavitaCfg = config.services.kavita;

  kavitaConfigJson = builtins.toJSON {
    kavitaUrl = serviceCfg.internalUrl;
    adminPasswordFile = "${kavitaCfg.dataDir}/credentials/admin-password";

    server = {
      hostName = serviceCfg.publicUrl;
      allowStatCollection = false;
      enableFolderWatching = true;
    };

    # OIDC API settings (database, not appsettings.json)
    oidc = {
      buttonText = "Login with ${oidcCfg.provider.displayName}";
      provisionAccounts = true;
      syncUserSettings = false;
      rolesClaim = "groups";
      rolesPrefix = "kavita-";
      defaultRoles = [ "Login" "Download" "Bookmark" "library-Books" "library-Comics" "library-Manga" ];
      autoLogin = true;
      disablePasswordAuth = true;
    };

    # type: 0=Manga, 1=Comic, 2=Book, 3=Image, 4=LightNovel
    # fileGroupTypes: 0=Archive, 1=EPUB, 2=PDF, 3=Image
    # FIXME: Let's extract this to "public library" and "private library" (only specific users are allowed to use it)
    libraries = [
      { name = "Books";   type = 2; folders = [ "/mnt/kavita/books" ];  fileGroupTypes = [ 1 2 ]; }
      { name = "Comics";  type = 1; folders = [ "/mnt/kavita/comics" ]; fileGroupTypes = [ 0 ];   }
      { name = "Manga";   type = 0; folders = [ "/mnt/kavita/manga" ];  fileGroupTypes = [ 0 ];   }
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
    };
    environment = {
      KAVITA_URL = serviceCfg.internalUrl;
      KAVITA_CONFIG_FILE = pkgs.writeText "kavita-config.json" kavitaConfigJson;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "kavita-configure" ./kavita-configure.nu}'';
  };
}
