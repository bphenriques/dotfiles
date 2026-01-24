{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.pocket-id;

  encryptionKeyFile = "/var/lib/pocket-id/encryption.key";
  apiKeyFile = config.sops.templates."pocket-id-api-key".path;
  smtpPasswordFile = config.sops.templates."pocket-id-smtp-password".path;
in
{
  imports = [ ./setup.nix ];

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
      ACCENT_COLOR = "default";
      STATIC_API_KEY_FILE = apiKeyFile;

      # SMTP configuration
      SMTP_HOST = self.settings.smtp.host;
      SMTP_PORT = toString self.settings.smtp.port;
      SMTP_FROM = self.settings.smtp.from;
      SMTP_USER = self.settings.smtp.user;
      SMTP_TLS = self.settings.smtp.tls;
      SMTP_PASSWORD_FILE = smtpPasswordFile;

      # invite-only, therefore the emails are valid for me.
      ALLOW_USER_SIGNUPS = false;
      EMAILS_VERIFIED = true;
      EMAIL_VERIFICATION_ENABLED = false;

      # Enable admin-triggered invite emails
      EMAIL_ONE_TIME_ACCESS_AS_ADMIN_ENABLED = true;
      EMAIL_ONE_TIME_ACCESS_AS_UNAUTHENTICATED_ENABLED = true; # FIXME:
    };
  };

  sops = {
    secrets.pocket_id_api_key = { };
    secrets.pocket_id_smtp_password = { }; # https://myaccount.google.com/apppasswords
    templates."pocket-id-api-key" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder.pocket_id_api_key;
    };
    templates."pocket-id-smtp-password" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder.pocket_id_smtp_password;
    };
  };

  systemd.services.pocket-id.preStart = ''
    if [ ! -f "${encryptionKeyFile}" ]; then
      mkdir -p "$(dirname "${encryptionKeyFile}")"
      ${lib.getExe pkgs.openssl} rand -base64 32 > "${encryptionKeyFile}"
      chown ${config.services.pocket-id.user}:${config.services.pocket-id.group} "${encryptionKeyFile}"
    fi
  '';
}
