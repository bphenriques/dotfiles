# TODO: Add ./immich to services/default.nix imports
# TODO: Add immich_oidc_client_secret and immich_api_key to secrets.yaml
# TODO: Register OIDC client in Pocket-ID:
#       - Client ID: immich
#       - Redirect URIs:
#         - https://immich.<domain>/auth/login
#         - https://immich.<domain>/user-settings
#         - app.immich:///oauth-callback
{ config, pkgs, lib, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
  serviceCfg = config.custom.home-server.services.immich;
  apiKeyFile = config.sops.secrets.immich_api_key.path;

  # Filter users with immich enabled and format for init script
  enabledUsers = lib.filterAttrs (_: u: u.services.immich.enable) config.custom.home-server.users;
  users = lib.mapAttrsToList (_: u: {
    inherit (u) email name;
  }) enabledUsers;

  libraries = [
    { name = "bphenriques-library"; ownerEmail = "bphenriques@example.com"; importPaths = [ "/mnt/media/bphenriques" ]; exclusionPatterns = []; }
    { name = "bphenriques-inbox"; ownerEmail = "bphenriques@example.com"; importPaths = [ "/mnt/media/bphenriques-inbox" ]; exclusionPatterns = []; }
  ];
in
{
  custom.home-server.services.immich.port = 2283;

  services.immich = {
    enable = true;
    host = "127.0.0.1";
    port = serviceCfg.port;
    mediaLocation = "/var/lib/immich";

    settings = {
      server.externalDomain = serviceCfg.publicUrl;
      newVersionCheck.enabled = false;

      # OAuth/OIDC via Pocket-ID
      oauth = {
        enabled = true;
        issuerUrl = config.custom.home-server.services.pocket-id.publicUrl;
        clientId = "immich";
        clientSecret._secret = config.sops.secrets.immich_oidc_client_secret.path;
        scope = "openid email profile";
        signingAlgorithm = "RS256";
        buttonText = "Login with PocketId";
        autoRegister = true;
        autoLaunch = false;
      };

      passwordLogin.enabled = true;

      # Preserve user storage structure
      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };
    };
  };

  # Add immich user to homelab group for NAS access
  users.users.immich.extraGroups = [ config.users.groups.homelab-bphenriques.name ];

  # Depend on NAS mount and bind external photo libraries into Immich's sandbox
  systemd.services.immich-server = {
    requires = [ homelabMounts.bphenriques.automountUnit ];
    after = [ homelabMounts.bphenriques.automountUnit ];
    serviceConfig.BindReadOnlyPaths = [
      "${pathsCfg.bphenriques.photos.library}:/mnt/media/bphenriques:ro"
      "${pathsCfg.bphenriques.photos.inbox}:/mnt/media/bphenriques-inbox:ro"
    ];
  };

  sops.secrets = {
    immich_oidc_client_secret = { };
    immich_api_key.owner = config.services.immich.user;
  };

  # Initialize users and external libraries after Immich starts
  systemd.services.immich-init = {
    description = "Initialize Immich users and external libraries";
    wantedBy = [ "multi-user.target" ];
    after = [ "immich-server.service" ];
    requires = [ "immich-server.service" ];
    partOf = [ "immich-server.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = config.services.immich.user;
      Group = config.services.immich.group;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    environment = {
      IMMICH_URL = "http://127.0.0.1:${toString serviceCfg.port}";
      IMMICH_API_KEY_FILE = apiKeyFile;
      IMMICH_USERS_JSON = builtins.toJSON users;
      IMMICH_LIBRARIES_JSON = builtins.toJSON libraries;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${./immich-init.nu}'';
  };
}
