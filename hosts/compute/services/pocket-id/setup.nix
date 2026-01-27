{ config, pkgs, lib, self, ... }:
let
  nuLib = import ../lib/nu.nix { inherit pkgs lib; };
  serviceCfg = config.custom.home-server.routes.pocket-id;
  oidcCfg = config.custom.home-server.oidc;
  apiKeyFile = config.sops.templates."pocket-id-api-key".path;

  enabledUsers = lib.filterAttrs (_: u: u.services.pocket-id.enable) config.custom.home-server.users;
  users = lib.mapAttrsToList (_: u: {
    inherit (u) username email firstName lastName name;
    groups = u.services.pocket-id.groups;
    isAdmin = builtins.elem "admins" u.services.pocket-id.groups;
  }) enabledUsers;

  allGroups = lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.services.pocket-id.groups) enabledUsers));
  groups = map (name: { inherit name; }) allGroups;

  # OIDC clients from pocket-id module - now with SOPS-backed credentials
  oidcClients = lib.mapAttrsToList (_: client: {
    inherit (client) name callbackURLs;
    pkceEnabled = client.pkce;
    credentialsFile = config.sops.secrets.${client.credentialsSecret}.path;
  }) oidcCfg.clients;

  configFile = pkgs.writeText "pocket-id-config.json" (builtins.toJSON {
    inherit users groups;
    clients = oidcClients;
  });

  initScript = self.lib.builders.writeNushellScript "pocket-id-init" ./pocket-id-init.nu;
  inviteScript = self.lib.builders.writeNushellScript "pocket-id-invite" ./pocket-id-invite.nu;
in
{
  config = lib.mkIf config.services.pocket-id.enable {
    # Ensure Pocket-ID can read each client's SOPS secret
    sops.secrets = lib.listToAttrs (lib.mapAttrsToList (_: client:
      lib.nameValuePair client.credentialsSecret {
        owner = config.services.pocket-id.user;
        group = config.services.pocket-id.group;
      }) oidcCfg.clients);

    systemd.services.pocket-id-init = {
      description = "Initialize Pocket ID users and groups";
      wantedBy = [ "multi-user.target" ];
      after = [ "pocket-id.service" ];
      requires = [ "pocket-id.service" ];
      partOf = [ "pocket-id.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = config.services.pocket-id.user;
        Group = config.services.pocket-id.group;
        Restart = "on-failure";
        RestartSec = 10;
        StartLimitBurst = 3;
      };
      environment = {
        POCKET_ID_URL = serviceCfg.internalUrl;
        POCKET_ID_API_KEY_FILE = apiKeyFile;
        POCKET_ID_CONFIG_FILE = configFile;
      };
      path = [ pkgs.nushell ];
      script = ''nu ${initScript}'';
    };

    environment.systemPackages = [
      (pkgs.writeShellScriptBin "pocket-id-invite" ''
        export POCKET_ID_URL="${serviceCfg.internalUrl}"
        export POCKET_ID_API_KEY_FILE="${apiKeyFile}"
        exec ${lib.getExe pkgs.nushell} ${inviteScript} "$@"
      '')
    ];
  };
}
