{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  kavitaCfg = config.services.kavita;

  # Default roles for new users (OIDC provisioned)
  defaultUserRoles = [ "Login" "Download" "Bookmark" ];

  # Libraries available to all users
  publicLibraries = [ "Books" "Comics" "Manga" ];

  enabledUsers = lib.filterAttrs (_: u: u.services.kavita.enable) config.custom.homelab.users;
  localUsers = lib.filterAttrs (_: u: u.services.kavita.passwordFile != null) enabledUsers;

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
      # AgeRating -1 = NotApplicable (no restriction)
      defaultAgeRestriction = -1;
      defaultIncludeUnknowns = true;
      autoLogin = false;
      disablePasswordAuth = false;
    };

    # type: 0=Manga, 1=Comic, 2=Book, 3=Image, 4=LightNovel
    libraries = let
      fileType = { archive = 1; epub = 2; pdf = 3; image = 4; };
    in [
      { name = "Books";   type = 2; folders = [ "/mnt/kavita/books" ];  fileGroupTypes = [ fileType.epub fileType.pdf ]; }
      { name = "Comics";  type = 1; folders = [ "/mnt/kavita/comics" ]; fileGroupTypes = [ fileType.archive ];           }
      { name = "Manga";   type = 0; folders = [ "/mnt/kavita/manga" ];  fileGroupTypes = [ fileType.archive ];           }
    ];

    # Local users provisioned with password authentication (all get public libraries)
    localUsers = lib.mapAttrsToList (_: u: {
      username = u.username;
      email = u.email;
      passwordCredential = "kavita-password-${u.username}";
      roles = [ "Login" ];
      libraries = publicLibraries;
    }) localUsers;
  });
in
{
  systemd.services.kavita-configure = {
    description = "Kavita setup";
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
      TimeoutStartSec = 600;
      User = kavitaCfg.user;
      Group = kavitaCfg.user;
      Restart = "on-failure";
      RestartSec = 10;
      LoadCredential = lib.mapAttrsToList (_: u:
        "kavita-password-${u.username}:${u.services.kavita.passwordFile}"
      ) localUsers;
    };
    environment = {
      KAVITA_URL = serviceCfg.url;
      KAVITA_CONFIG_FILE = kavitaConfigFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "kavita-configure" ./kavita-configure.nu}'';
  };
}
