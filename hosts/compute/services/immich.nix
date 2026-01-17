# TODO: Add ./immich.nix to services/default.nix imports
# TODO: Add immich_oidc_client_secret to secrets.yaml
# TODO: Register OIDC client in Pocket-ID:
#       - Client ID: immich
#       - Redirect URIs:
#         - https://immich.<domain>/auth/login
#         - https://immich.<domain>/user-settings
#         - app.immich:///oauth-callback
{ config, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
in
{
  custom.home-server.services.immich.port = 2283;

  services.immich = {
    enable = true;
    host = "127.0.0.1";
    port = config.custom.home-server.services.immich.port;
    mediaLocation = "/var/lib/immich";

    settings = {
      server.externalDomain = config.custom.home-server.services.immich.publicUrl;
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

  sops.secrets.immich_oidc_client_secret = { };
}
