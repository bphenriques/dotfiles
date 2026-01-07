{ config, ... }:
let
  port = 8082;
in
{
  custom.home-server.services.pocket-id = {
    internalUrl = "http://127.0.0.1:${toString port}";
  };

  services.pocket-id = {
    enable = true;
    settings = {
      APP_URL = "${config.custom.home-server.services.pocket-id.publicUrl}";
      PORT = (toString port);
      HOST = "127.0.0.1";
      TRUST_PROXY = true;
      ANALYTICS_DISABLED = true;
      ENCRYPTION_KEY_FILE = "/var/lib/pocket-id/encryption.key";
      EMAILS_VERIFIED = true;
      SIGNUP_DEFAULT_USER_GROUP_IDS = "users";
      ACCENT_COLOR = "default";
    };
  };

  systemd.services.pocket-id = {
    preStart = ''
      if [ ! -f /var/lib/pocket-id/encryption.key ]; then
        mkdir -p /var/lib/pocket-id
        openssl rand -base64 32 > /var/lib/pocket-id/encryption.key
        chown ${config.services.pocket-id.user}:${config.services.pocket-id.group} /var/lib/pocket-id/encryption.key
      fi
    '';
  };

  # I can install pocket-id cli to generate a one-time password.
  # Automatic provisioning is blocked by https://github.com/pocket-id/pocket-id/pull/1205
}