{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.pocket-id;
  oidcCfg = config.custom.home-server.oidc;
  apiKeyFile = config.sops.templates.pocket-id-api-key.path;

  enabledUsers = config.custom.home-server.enabledUsers.pocket-id;
  settings = (builtins.toJSON {
    # TODO: Add profile picture
    users = lib.mapAttrsToList (_: u: {
      inherit (u) username email firstName lastName name isAdmin groups;
    }) enabledUsers;

    groups = map (name: { inherit name; }) (lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.groups) enabledUsers)));

    # TODO: Logo (light and dark)
    clients = lib.mapAttrsToList (_: client: {
      inherit (client) name callbackURLs;
      pkceEnabled = client.pkce;
    }) oidcCfg.clients;
  });
in
{
  config = lib.mkIf config.services.pocket-id.enable {
    systemd.services.pocket-id-configure = {
      description = "Configure Pocket ID users and groups";
      wantedBy = [ "multi-user.target" ];
      after = [ "pocket-id.service" ];
      requires = [ "pocket-id.service" ];
      partOf = [ "pocket-id.service" ];
      restartTriggers = [ settings ];
      serviceConfig = {
        Type = "oneshot";
        # Run as root to set proper file ownership for OIDC credentials
        Restart = "on-failure";
        RestartSec = 10;
        StartLimitBurst = 3;
      };
      environment = {
        POCKET_ID_URL = serviceCfg.internalUrl;
        POCKET_ID_API_KEY_FILE = apiKeyFile;
        POCKET_ID_CONFIG_FILE = pkgs.writeText "pocket-id-config.json" settings;
        HOMELAB_OIDC_CREDENTIALS_DIR = oidcCfg.credentials.dir;
        HOMELAB_OIDC_PLACEHOLDER = oidcCfg.credentials.placeholder;
      };
      path = [ pkgs.nushell ];
      script = ''nu ${self.lib.builders.writeNushellScript "pocket-id-configure" ./pocket-id-configure.nu}'';
    };

    environment.systemPackages = [
      (pkgs.writeShellScriptBin "pocket-id-invite" ''
        export POCKET_ID_URL="${serviceCfg.internalUrl}"
        export POCKET_ID_API_KEY_FILE="${apiKeyFile}"
        exec ${lib.getExe pkgs.nushell} ${self.lib.builders.writeNushellScript "pocket-id-invite" ./pocket-id-invite.nu} "$@"
      '')
    ];
  };
}
