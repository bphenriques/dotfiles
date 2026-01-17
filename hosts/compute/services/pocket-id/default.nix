{ config, pkgs, ... }:
let
  serviceCfg = config.custom.home-server.services.pocket-id;
  encryptionKeyFile = "/var/lib/pocket-id/encryption.key";
  apiKeyFile = config.sops.templates."pocket-id-api-key".path;

  groups = [
    { name = "admins"; }
    { name = "users"; }
  ];

  users = [
    { username = "bphenriques"; email = "bphenriques@example.com"; firstName = "Bruno"; lastName = "Henriques"; isAdmin = true; groups = [ "admins" "users" ]; }
  ];
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
    templates."pocket-id-api-key".content = config.sops.placeholder.pocket_id_api_key;
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
    after = [ "pocket-id.service" ];
    requires = [ "pocket-id.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
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
