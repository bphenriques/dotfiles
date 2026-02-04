{ config, pkgs, lib, self, ... }:
let
  nuLib = import ../lib/nu.nix { inherit pkgs lib; };
  serviceCfg = config.custom.home-server.routes.pocket-id;
  oidcCfg = config.custom.home-server.oidc;
  apiKeyFile = config.sops.templates.pocket-id-api-key.path;

  enabledUsers = config.custom.home-server.enabledUsers.pocket-id;
  settings = {
    # TODO: Add profile picture
    users = lib.mapAttrsToList (_: u: {
      inherit (u) username email firstName lastName name isAdmin;
      groups = u.services.pocket-id.groups;
    }) enabledUsers;

    groups = map (name: { inherit name; }) (lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.services.pocket-id.groups) enabledUsers)));

    # TODO: Logo (light and dark)
    clients = lib.mapAttrsToList (_: client: {
      inherit (client) name callbackURLs;
      pkceEnabled = client.pkce;
    }) oidcCfg.clients;
  };
in
{
  config = lib.mkIf config.services.pocket-id.enable {
    systemd.services.pocket-id-init = {
      description = "Initialize Pocket ID users and groups";
      wantedBy = [ "multi-user.target" ];
      after = [ "pocket-id.service" ];
      requires = [ "pocket-id.service" ];
      partOf = [ "pocket-id.service" ];
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
        POCKET_ID_CONFIG_FILE = pkgs.writeText "pocket-id-config.json" (builtins.toJSON settings);
        HOMELAB_OIDC_CREDENTIALS_DIR = oidcCfg.credentials.dir;
        HOMELAB_OIDC_PLACEHOLDER = oidcCfg.credentials.placeholder;
      };
      path = [ pkgs.nushell ];
      script = ''nu ${self.lib.builders.writeNushellScript "pocket-id-init" ./pocket-id-init.nu}'';
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
