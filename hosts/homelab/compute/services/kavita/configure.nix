{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  kavitaCfg = config.services.kavita;

  # Default roles for new users (OIDC provisioned)
  defaultUserRoles = [ "Login" "Download" "Bookmark" ];

  # Libraries available to all users
  publicLibraries = [ "Books" "Comics" "Manga" ];

  # TODO: admin could be removed if Kavita supports OIDC-only admin
  kavitaConfigFile = pkgs.writeText "kavita-config.json" (builtins.toJSON {
    kavitaUrl = serviceCfg.url;
    adminUsername = "admin";
    adminPasswordFile = serviceCfg.secrets.files.admin-password.path;

    server = {
      hostName = serviceCfg.publicUrl;
      allowStatCollection = false;
      enableFolderWatching = true;
    };

    # OIDC for authentication only - roles managed via Kavita API
    oidc = {
      buttonText = "Login with ${oidcCfg.provider.displayName}";
      provisionAccounts = true;
      syncUserSettings = false;  # Roles managed via API, not OIDC claims
      defaultRoles = defaultUserRoles;
      defaultLibraries = publicLibraries;
      autoLogin = true;
      disablePasswordAuth = true;
    };

    # type: 0=Manga, 1=Comic, 2=Book, 3=Image, 4=LightNovel
    # fileGroupTypes: 0=Archive, 1=EPUB, 2=PDF, 3=Image
    libraries = [
      { name = "Books";   type = 2; folders = [ "/mnt/kavita/books" ];  fileGroupTypes = [ 1 2 ]; }
      { name = "Comics";  type = 1; folders = [ "/mnt/kavita/comics" ]; fileGroupTypes = [ 0 ];   }
      { name = "Manga";   type = 0; folders = [ "/mnt/kavita/manga" ];  fileGroupTypes = [ 0 ];   }
    ];

    # Per-user overrides (for private library access). Example:
    # users = [
    #   { email = "user@example.com"; roles = defaultUserRoles; libraries = publicLibraries ++ [ "PrivateLib" ]; }
    # ];
    users = [];
  });
in
{

  systemd.services.kavita-configure = {
    description = "Configure Kavita OIDC and libraries";
    wantedBy = [ "kavita.service" ];
    after = [ "kavita.service" ];
    requires = [ "kavita.service" ];
    partOf = [ "kavita.service" ];
    restartTriggers = [ kavitaConfigFile ./kavita-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = kavitaCfg.user;
      Group = kavitaCfg.user;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      KAVITA_URL = serviceCfg.url;
      KAVITA_CONFIG_FILE = kavitaConfigFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "kavita-configure" ./kavita-configure.nu}'';
  };
}
