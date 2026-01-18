{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.home-server.services.pocket-id;
  encryptionKeyFile = "/var/lib/pocket-id/encryption.key";
  apiKeyFile = config.sops.templates."pocket-id-api-key".path;

  # Filter users with pocket-id enabled and format for init script
  enabledUsers = lib.filterAttrs (_: u: u.services.pocket-id.enable) config.custom.home-server.users;
  users = lib.mapAttrsToList (_: u: {
    inherit (u) username email;
    firstName = u.name;
    lastName = "";
    groups = u.services.pocket-id.groups;
    isAdmin = builtins.elem "admins" u.services.pocket-id.groups;
  }) enabledUsers;

  # Collect all unique groups from users
  allGroups = lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.services.pocket-id.groups) enabledUsers));
  groups = map (name: { inherit name; }) allGroups;
in
{
  custom.home-server.services.pocket-id.port = 8082;

  services.pocket-id = {
    enable = true;
    settings = {
      APP_URL = serviceCfg.publicUrl;
      PORT = toString serviceCfg.port;
      HOST = "127.0.0.1";
      TRUST_PROXY = true;
      ANALYTICS_DISABLED = true;
      ENCRYPTION_KEY_FILE = encryptionKeyFile;
      EMAILS_VERIFIED = true;
      ACCENT_COLOR = "default";
      STATIC_API_KEY_FILE = apiKeyFile;
    };
  };

  sops = {
    secrets.pocket_id_api_key = { };
    templates."pocket-id-api-key" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder.pocket_id_api_key;
    };
  };

  systemd.services.pocket-id = {
    preStart = ''
      if [ ! -f "${encryptionKeyFile}" ]; then
        mkdir -p "$(dirname "${encryptionKeyFile}")"
        openssl rand -base64 32 > "${encryptionKeyFile}"
        chown ${config.services.pocket-id.user}:${config.services.pocket-id.group} "${encryptionKeyFile}"
      fi
    '';
  };

  # Initialize users and groups after Pocket ID starts
  systemd.services.pocket-id-init = {
    description = "Initialize Pocket ID users and groups";
    wantedBy = [ "multi-user.target" ]; # Start only after this one.
    after = [ "pocket-id.service" ];    # Start right after this one.
    requires = [ "pocket-id.service" ]; # Not 'wants' as I would consider pocket-id as broken if the init fails.
    partOf = [ "pocket-id.service" ];   # Restart when this one restarts
    serviceConfig = {
      Type = "oneshot";
      User = config.services.pocket-id.user;
      Group = config.services.pocket-id.group;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    environment = {
      POCKET_ID_URL = "http://127.0.0.1:${toString serviceCfg.port}";
      POCKET_ID_API_KEY_FILE = apiKeyFile;
      POCKET_ID_GROUPS_JSON = builtins.toJSON groups;
      POCKET_ID_USERS_JSON = builtins.toJSON users;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${./pocket-id-init.nu}'';
  };
}

# TODO: OIDC clients declarative setup
