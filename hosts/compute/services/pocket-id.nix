{ config, ... }:
let encryptionKeyFile = "/var/lib/pocket-id/encryption.key";
in
{
  custom.home-server.services.pocket-id.port = 8082;

  services.pocket-id = {
    enable = true;
    settings = {
      APP_URL = config.custom.home-server.services.pocket-id.publicUrl;
      PORT = (toString config.custom.home-server.services.pocket-id.port);
      HOST = "127.0.0.1";
      TRUST_PROXY = true;
      ANALYTICS_DISABLED = true;
      ENCRYPTION_KEY_FILE = encryptionKeyFile;
      EMAILS_VERIFIED = true;
      SIGNUP_DEFAULT_USER_GROUP_IDS = "users";
      ACCENT_COLOR = "default";
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
}

# Wait for pocket 2.2.0 where I can set a static api key
# TODO: wallpaper
# User provisioning?
# OIDC client
